#' Read an SSSOM Mapping Set
#'
#' Reads an SSSOM/TSV file with embedded or external metadata into an
#' `sssom_mapping_set` (a modified [tibble][tibble::tibble]).
#'
#' @param sssom The path to an SSSOM/TSV file, as a string.
#' @param external_metadata The path to a file containing the
#' [external metadata](https://mapping-commons.github.io/sssom/spec-formats-tsv/#external-metadata-mode)
#' for this mapping file, as a string. Use `NULL` (default) to parse embedded
#' metadata.
#' @param metadata_missing The signal to give when embedded metadata is missing,
#' as a string. One of "warning" (default), "error", or "none". Ignored when
#' `external_metadata` is specified.
#' @param unofficial_cols Response desired when columns outside the SSSOM spec
#' are found in the file, as a string. One of "error" (default), "warning",
#' "other" (meaning insert them into an `other` column formatted
#' as pipe-separated, key-value pairs as described in the
#' [SSSOM spec](https://mapping-commons.github.io/sssom/other/); moved columns
#' will be identified in a message), or "allow" (i.e. retain them without any
#' notification).
#'
#' @family SSSOM-related functions
#' @export
read_sssom <- function(sssom, external_metadata = NULL,
                       metadata_missing = "warning",
                       unofficial_cols = "error") {
    unofficial_cols <- match.arg(
        unofficial_cols,
        choices = c("error", "warning", "allow", "other")
    )

    if (is.null(external_metadata)) {
        sssom_complete <- unembed_sssom_metadata(sssom, missing = metadata_missing)
        sssom <- sssom_complete$mapping
        metadata <- sssom_complete$metadata
    } else {
        metadata <- drop_blank(readr::read_lines(external_metadata))
        metadata <- vctr_to_string(metadata, delim = "\n")
    }

    out <- readr::read_tsv(
        sssom,
        col_types = readr::cols(
            .default = readr::col_character()
        )
    )
    official_types <- .sssom_slot_types[names(out) %in% .sssom_mapping_slots]

    other_cols <- names(out)[!names(out) %in% .sssom_mapping_slots]
    if (length(other_cols) > 0 && unofficial_cols != "allow") {
        if (length(other_cols) > 3 && unofficial_cols != "other") {
            cols_report <- paste0(
                vctr_to_string(other_cols[1:3], delim = ", "),
                ", (+", length(other_cols) - 3, ")"
            )
        } else {
            cols_report <- vctr_to_string(other_cols, delim = ", ")
        }

        if (unofficial_cols == "other") {
            rlang:inform(
                paste0(
                    "The following data were added to the 'other' column: ",
                    cols_report
                )
            )
            out <- dplyr::mutate(
                out,
                dplyr::across(
                    other_cols,
                    paste0(dplyr::cur_column(), ": ", .x)
                )
            )
            if ("other" %in% names(out)) {
                other_cols <- c(other, other_cols)
            }
            out <- dplyr::unite(out, col = "other", {{ other_cols }}, sep = "|")
        } else {
            rlang::signal(
                paste0("Unofficial columns exist in SSSOM data:", cols_report),
                class = unofficial_cols
            )
        }
    }

    attr(out, "metadata") <- metadata
    class(out) <- c("sssom_mapping_set", class(out))
    out
}


validate_sssom <- function(sssom) {
    c("uri" = "c", "double" = "d", "date" = "D", "string" = "c", "mapping set reference",
      "EntityReference", "mapping", "entity_type_enum", "predicate_modifier_enum",
      "mapping_cardinality_enum")
}

#' Revise Ontology Mappings from SSSOM
#'
#' Updates the mappings of an input ontology using the mappings from an SSSOM
#' file. The SSSOM specification is _not_ currently supported in its entirety.
#' Only one mapping record should exist for each triple.
#'
#' @param input
#' @param exact_as_xref Whether to treat `oboInOwl:hasDbXref` as equivalent to
#' `skos:exactMatch` (default: `TRUE`).
#'
#' @family SSSOM-related functions
#' @export
sssom_revise <- function(input, sssom, xref_as_exact = TRUE, replaced_out = NULL) {
    map_up <- read_sssom(sssom)
    out <- list(map_init = map_up)
    if (isTRUE(exact_as_xref)) {
        map_xref <- map_up %>%
            dplyr::filter(predicate_id == "skos:exactMatch") %>%
            dplyr::mutate(predicate_id == "oboInOwl:hasDbXref")
        map_up <- dplyr::bind_rows(map_up, map_xref)
    }

    map_exist <- extract_mappings(input)

    map_ignore <- dplyr::semi_join(
        map_up,
        map_exist,
        by = c("subject_id", "predicate_id", "predicate_modifier", "object_id")
    )
    out["map_exist"] <- map_exist
    out["map_ignore"] <- map_ignore
    out
}


# helpers -----------------------------------------------------------------

#' Helper for read_sssom()
#'
#' Separates embedded metadata from SSSOM mapping set.
#'
#' @inheritParams read_sssom
#' @param missing  The signal to give when metadata is missing, as a string.
#' One of "warning" (default), "error", or "none".
#'
#' @noRd
unembed_sssom_metadata <- function(file, missing = "warning") {
    missing <- match.arg(
        missing,
        choices = c("warning", "error", "none")
    )
    raw <- readr::read_lines(file)
    metadata_rng <- stringr::str_detect(raw, "^#") %>%
        which()

    if (length(metadata_rng) == 0) {
        if (missing != "ignore") {
            rlang::signal(
                class = missing,
                "No metadata was identified in this SSSOM file."
            )
        }
        out <- list(
            metadata = NA,
            mapping = I(raw)
        )
        return(out)
    }

    metadata_raw <- raw[min(metadata_rng):max(metadata_rng)]
    n_space <- stringr::str_remove(metadata_raw, "^#") %>%
        stringr::str_remove("[^[:space:]].+") %>%
        stringr::str_count(" ") %>%
        min()
    trim_pattern <- paste0("^# {", n_space, "}")
    out <- list(
        metadata = vctr_to_string(
            stringr::str_remove(metadata_raw, trim_pattern),
            delim = "\n"
        ),
        mapping = I(raw[(max(metadata_rng) + 1):length(raw)])
    )
    out
}

# extra -------------------------------------------------------------------



    # header <- raw[max(metadata_rng) + 1] %>%
    #     stringr::str_split("\t") %>%
    #     unlist()
    #
    #
    # %>%
    #     stringr::str_replace("(1[,-]?[0-9]*).*", "\\1") %>%
    #     stringr::str_split("[,-]") %>%
    #     .[[1]] %>%
    #     as.integer() %>%
    #
    #     metadata_rng <- which(stringr::str_detect(raw, "^#"))
    # metadata_discont <- metadata_rng - 1 != dplyr::lag(1:10, default = 0)
    # if (any(metadata_discont) {
    #     rlang::abort(
    #         "SSSOM metadata "
    #     )
    # }
    #
    # metadata <- stringr::str_remove(raw[metadata_rng], "^# *")
