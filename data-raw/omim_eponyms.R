# Update `disease_eponyms_curated.tsv` with OMIM capitalization candidates
#
# Downloads mimTitles.txt and genemap2.txt from OMIM, builds a capitalized
# word vocabulary from genemap2 phenotype names, intersects it with words
# appearing in all mimTitles entries, and merges the results into the shared
# disease_eponyms_curated.tsv curation file.
#
# SECURITY: The OMIM API key is read exclusively from the OMIM_API_KEY set in a
# credentials store.
#
# Maintainer Workflow:
#   1. Set OMIM_API_KEY (only once): `keyring::key_set("OMIM_API_KEY")`
#   2. source("data-raw/omim_eponyms.R")
#   3. Review new "pending" rows in data-raw/disease_eponyms_curated.tsv
#      and set status to "cap" or "lower" (see description below).
#   4. Run data-raw/build_disease_eponyms.R to rebuild the dataset.
#
#
# OUTPUT: disease_eponyms_curated.tsv is updated by multiple source-specific
# mining scripts. This script only adds/refreshes OMIM-sourced rows.
#
# The curated TSV columns:
#   word_lower  - lowercase word (primary key, unique across all sources)
#   word_cap    - correctly capitalized form; NA for contested entries (curator
#                 must fill in manually before setting status to "cap")
#   alt_caps    - competing cap forms with counts, e.g. "MacLeod (48); Macleod (2)"
#                 (OMIM rows: refreshed when word found in current mining;
#                  preserved when word absent from mining; other sources: untouched)
#   examples    - up to 3 source phenotype names where the word was seen
#                 (OMIM rows: refreshed when word found in current mining;
#                  preserved when word absent from mining; other sources: untouched)
#   status      - "cap" (capitalize in output) | "lower" (leave lowercase) | "pending"
#   source      - provenance of the candidate word (e.g. "OMIM")
#   notes       - free-text annotation field (optional)

devtools::load_all()

# ── 0. Configuration ──────────────────────────────────────────────────────────

data_raw_dir <- here::here("data-raw")
download_dir <- file.path(data_raw_dir, "downloads")
curated_tsv <- file.path(data_raw_dir, "disease_eponyms_curated.tsv")

dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)

api_key <- keyring::key_get("OMIM_API_KEY")
if (!nzchar(api_key)) {
    rlang::abort(
        c(
            "OMIM API key not found in credentials store.",
            i = 'Use keyring::key_set("OMIM_API_KEY") and paste the API key when prompted'
        )
    )
}


# ── 1. Download OMIM files ────────────────────────────────────────────────────

mim_file <- file.path(download_dir, "mimTitles.txt")
gmap_file <- file.path(download_dir, "genemap2.txt")

# Skip files downloaded within the last 30 days.
# Age is determined from the "# Generated: YYYY-MM-DD" header line, which OMIM
# sets to the download time. This survives version-control checkouts, unlike
# file.mtime().
omim_header_date <- function(path) {
    header <- readLines(path, n = 20L, warn = FALSE)
    date_line <- grep("^#\\s*Generated:", header, value = TRUE)
    if (length(date_line) == 0L) {
        rlang::abort(c(
            paste0(
                "Cannot determine age of '",
                basename(path),
                "': no 'Generated:' line found in first 20 lines."
            ),
            i = "Delete the file and re-run to force a fresh download."
        ))
    }
    date_str <- stringr::str_extract(date_line[[1L]], "\\d{4}-\\d{2}-\\d{2}")
    if (is.na(date_str)) {
        rlang::abort(c(
            paste0(
                "Cannot parse date from header of '",
                basename(path),
                "': '",
                date_line[[1L]],
                "'"
            ),
            i = "Delete the file and re-run to force a fresh download."
        ))
    }
    as.Date(date_str)
}

omim_age_days <- function(path) {
    if (!file.exists(path)) {
        return(Inf)
    }
    as.numeric(Sys.Date() - omim_header_date(path))
}

to_download <- c(
    if (omim_age_days(mim_file) > 30) "mimTitles",
    if (omim_age_days(gmap_file) > 30) "genemap2"
)

if (length(to_download) > 0L) {
    message(
        "Downloading ",
        paste(to_download, collapse = " and "),
        " from OMIM..."
    )
    download_omim(
        to_download,
        dest_dir = download_dir,
        api_key = api_key
    )
} else {
    message(
        "OMIM files are recent (< 30 days old); skipping download.\n",
        "  mimTitles : ",
        omim_header_date(mim_file),
        "\n",
        "  genemap2  : ",
        omim_header_date(gmap_file)
    )
}


# ── 2. Load mimTitles ────────────────────────────────────────────────────────

mim_raw <- read_omim(mim_file) |>
    dplyr::filter(
        .data$prefix != "Caret",
        !is.na(.data$preferred_title_symbol)
    ) |>
    dplyr::mutate(
        entry = stringr::str_remove(.data$preferred_title_symbol, "\\s*;.*$") |>
            stringr::str_trim()
    )


# ── 3. Load genemap2 and extract mixed-case phenotype names ──────────────────

gmap_raw <- read_omim(gmap_file) |>
    dplyr::filter(!is.na(.data$phenotypes))

# Split multi-phenotype cells and extract clean names.
# genemap2 phenotype format: "Name, mim_number (mapping_key), Inheritance"
# Brackets/braces/? modify the entry type but are stripped here.
pheno_flat <- gmap_raw |>
    dplyr::transmute(
        pheno_list = stringr::str_split(.data$phenotypes, "\\s*;\\s*")
    ) |>
    tidyr::unnest("pheno_list") |>
    dplyr::transmute(
        pheno_name = stringr::str_remove(
            stringr::str_trim(.data$pheno_list),
            ",?\\s*\\d{6}.*$"
        ) |>
            stringr::str_remove_all("^[\\[\\{\\?]|[\\]\\}]$") |>
            stringr::str_trim()
    ) |>
    dplyr::filter(nzchar(.data$pheno_name)) |>
    dplyr::distinct()


# ── 4. Mine word-level capitalization differences ─────────────────────────────
#
# Strategy:
#   1. Build a capitalized vocabulary from ALL words (≥ 3 alpha chars) that
#      appear with initial-cap or all-caps in any genemap2 phenotype name.
#   2. Find which of those words also appear (lowercased) in any mimTitles
#      entry name — those are candidates for eponym replacement.
#
# This covers all ~28k mimTitles entries, not just those with genemap2 matches,
# and is simpler than per-row pair comparison.

cap_vocab <- pheno_flat |>
    dplyr::mutate(
        word = stringr::str_extract_all(.data$pheno_name, "[A-Za-z]{3,}")
    ) |>
    tidyr::unnest("word") |>
    dplyr::filter(stringr::str_detect(.data$word, "^[A-Z][a-z]|^[A-Z]{2,}")) |>
    dplyr::mutate(word_lower = stringr::str_to_lower(.data$word)) |>
    dplyr::rename("word_cap" = "word", "source" = "pheno_name") |>
    dplyr::select("word_lower", "word_cap", "source")

mim_words <- mim_raw |>
    dplyr::mutate(
        word_lower = stringr::str_extract_all(
            stringr::str_to_lower(.data$entry),
            "[A-Za-z]{3,}"
        )
    ) |>
    tidyr::unnest("word_lower") |>
    dplyr::distinct(.data$word_lower)

all_candidates <- cap_vocab |>
    dplyr::semi_join(mim_words, by = "word_lower")

if (nrow(all_candidates) == 0L) {
    message("No capitalization candidates found. Nothing to update.")
    candidate_df <- tibble::tibble(
        word_lower = character(0L),
        word_cap = character(0L),
        alt_caps = character(0L),
        examples = character(0L)
    )
} else {
    # Count occurrences
    counts_df <- all_candidates |>
        dplyr::count(.data$word_lower, .data$word_cap, name = "n") |>
        dplyr::arrange(.data$word_lower, dplyr::desc(.data$n))

    # Examples: up to 3 distinct source names per word_lower (any cap form),
    # drawn from the most frequent form first via the sorted counts join
    examples_df <- counts_df |>
        dplyr::left_join(
            dplyr::distinct(
                all_candidates,
                .data$word_lower,
                .data$word_cap,
                .data$source
            ),
            by = c("word_lower", "word_cap")
        ) |>
        dplyr::group_by(.data$word_lower) |>
        dplyr::slice_head(n = 3L) |>
        dplyr::summarize(
            examples = paste(
                stringr::str_trunc(.data$source, 60L),
                collapse = "; "
            ),
            .groups = "drop"
        )

    # Summarize per word_lower:
    #   uncontested (1 form)  → pre-fill word_cap; alt_caps = ""
    #   contested   (>1 form) → word_cap = NA; alt_caps lists all forms + counts
    # Contested entries are left blank so the curator must type the correct form,
    # preventing silent acceptance of a pre-filled value that may be wrong.
    candidate_df <- counts_df |>
        dplyr::group_by(.data$word_lower) |>
        dplyr::summarize(
            alt_caps = if (dplyr::n() > 1L) {
                paste(
                    paste0(.data$word_cap, " (", .data$n, ")"),
                    collapse = "; "
                )
            } else {
                ""
            },
            word_cap = if (dplyr::n() == 1L) {
                .data$word_cap[[1L]]
            } else {
                NA_character_
            },
            total_n = sum(.data$n),
            .groups = "drop"
        ) |>
        dplyr::left_join(examples_df, by = "word_lower") |>
        dplyr::arrange(dplyr::desc(.data$total_n)) |>
        dplyr::select("word_lower", "word_cap", "alt_caps", "examples")
}

message(
    "OMIM data: ",
    nrow(mim_raw),
    " mimTitles entries | ",
    nrow(pheno_flat),
    " genemap2 phenotypes \u2192 ",
    nrow(candidate_df),
    " candidates (",
    sum(nzchar(candidate_df$alt_caps) & !is.na(candidate_df$alt_caps)),
    " contested)"
)


# ── 5. Merge with existing curation state ─────────────────────────────────────

if (file.exists(curated_tsv)) {
    curated <- readr::read_tsv(
        curated_tsv,
        col_types = readr::cols(.default = "c"),
        na = ""
    )
    # Backfill source for TSVs created before this column was added; all
    # pre-existing rows are OMIM-sourced by definition.
    if (!"source" %in% names(curated)) {
        curated$source <- "OMIM"
    } else {
        curated$source <- dplyr::coalesce(curated$source, "OMIM")
    }

    # Candidates completely new to the curated file → add as "pending"
    new_rows <- candidate_df |>
        dplyr::anti_join(curated, by = "word_lower") |>
        dplyr::mutate(
            status = "pending",
            source = "OMIM",
            notes = NA_character_
        )

    # Existing rows: only refresh OMIM-sourced rows (other sources are untouched)
    #   - word_cap:              refresh only for pending OMIM rows
    #   - alt_caps and examples: refresh for OMIM rows found in current mining;
    #                            preserve existing values for words that have
    #                            dropped out of the mining results
    updated <- curated |>
        dplyr::left_join(
            dplyr::select(
                candidate_df,
                "word_lower",
                word_cap_new = "word_cap",
                alt_caps_new = "alt_caps",
                examples_new = "examples"
            ),
            by = "word_lower"
        ) |>
        dplyr::mutate(
            word_cap = dplyr::if_else(
                .data$source == "OMIM" &
                    .data$status == "pending" &
                    !is.na(.data$word_cap_new),
                .data$word_cap_new,
                .data$word_cap
            ),
            alt_caps = dplyr::if_else(
                .data$source == "OMIM" & !is.na(.data$alt_caps_new),
                .data$alt_caps_new,
                .data$alt_caps
            ),
            examples = dplyr::if_else(
                .data$source == "OMIM" & !is.na(.data$examples_new),
                .data$examples_new,
                .data$examples
            )
        ) |>
        dplyr::select(
            !dplyr::all_of(c("word_cap_new", "alt_caps_new", "examples_new"))
        )

    out_df <- dplyr::bind_rows(updated, new_rows)

    # Warn if a previously capped OMIM word is now contested: OMIM has started
    # spelling it inconsistently and the accepted form may not be correct.
    contested_accepted <- out_df |>
        dplyr::filter(
            .data$source == "OMIM",
            .data$status == "cap",
            nzchar(.data$alt_caps)
        )
    if (nrow(contested_accepted) > 0L) {
        rlang::warn(c(
            "Capped eponyms now have contested capitalization in OMIM:",
            setNames(
                paste0(
                    contested_accepted$word_lower,
                    ": ",
                    contested_accepted$alt_caps
                ),
                rep("!", nrow(contested_accepted))
            ),
            i = "Consider setting status to 'lower' and leaving these lowercase."
        ))
    }

    # Warn if previously capped words no longer appear as mining candidates
    # (OMIM may have renamed an entry)
    missing_accepted <- out_df |>
        dplyr::filter(
            .data$source == "OMIM",
            .data$status == "cap",
            !.data$word_lower %in% candidate_df$word_lower
        )
    if (nrow(missing_accepted) > 0L) {
        rlang::warn(c(
            "Capped eponyms no longer detected by candidate mining:",
            setNames(
                missing_accepted$word_lower,
                rep("!", nrow(missing_accepted))
            ),
            i = "Verify these are still needed or set their status to 'lower'."
        ))
    }
} else {
    message("No existing curated TSV found. Creating new one.")
    out_df <- candidate_df |>
        dplyr::mutate(
            status = "pending",
            source = "OMIM",
            notes = NA_character_
        )
}


# ── 6. Write updated curation TSV ─────────────────────────────────────────────

# reorder columns to match the curated TSV spec
out_df <- out_df |>
    dplyr::relocate(
        "word_lower",
        "word_cap",
        "alt_caps",
        "examples",
        "source",
        "status",
        "notes",
        .before = 1
    )

readr::write_tsv(out_df, curated_tsv, na = "", quote = "needed")
message(
    "Curated TSV written: ",
    curated_tsv,
    "\n",
    "  pending : ",
    sum(out_df$status == "pending", na.rm = TRUE),
    "\n",
    "  cap     : ",
    sum(out_df$status == "cap", na.rm = TRUE),
    "\n",
    "  lower   : ",
    sum(out_df$status == "lower", na.rm = TRUE),
    "\n",
    "\nNext step: review 'pending' rows in ",
    curated_tsv,
    "\n",
    "  Set status to 'cap' (capitalize) or 'lower' (leave lowercase),\n",
    "  then run data-raw/build_disease_eponyms.R to rebuild the dataset."
)
