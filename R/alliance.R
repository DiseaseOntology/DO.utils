#' Download Alliance .tsv.gz File
#'
#' Downloads a URL-specified .tsv.gz file from the Alliance of Genome
#' Resources. Files can be found at
#' <https://www.alliancegenome.org/downloads>. Right-click on the "tsv" link
#' of a desired file and select "Copy Link" to get the file URL.
#'
#' A date stamp indicating download date is added to the base file name.
#'
#' @section Recommendation:
#' Although it's possbile to directly read a file from the URL, downloading it
#' promotes reproducibility and ensures future access if needed.
#'
#' @param dest_dir path to directory where file will be saved
#' @param url URL to Alliance file; if not provided, will be requested at console
#'
#' @return
#' Path to saved file.
#'
#' @export
download_alliance_tsv <- function(dest_dir, url = NULL) {

    # Ask for URL if missing
    if (missing(url)) {
        url <- "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
    }

    dest_file <- file.path(dest_dir, basename(url))

    # avoid overwrite if file exists
    if (file.exists(dest_file)) {
        message(dest_file, " exists. Archiving...\n")
        file_version <- alliance_version(dest_file, as_string = TRUE)
        archive_file <- stringr::str_replace(
            dest_file,
            "\\.tsv\\.gz",
            paste0("-", file_version, ".tsv.gz")
        )
        # if archive already exists validate 2 files are identical & delete
        # instead of moving file (or fail if not identical)
        if (file.exists(archive_file)) {
            dest_file_md5 <- tools::md5sum(dest_file)
            archive_md5 <- tools::md5sum(archive_file)

            if (dest_file_md5 == archive_md5) {
                message("Archive file ", archive_file,
                        " already exists.\n Removing ", dest_file, "\n")

                file.remove(dest_file)
            } else {
                stop(
                    paste0("Archive file ", archive_file,
                    " already exists but md5sums differ. Aborting...")
                )
            }
        # if archive does not exist rename file with alliance version & file
        # datetime
        } else {
            message("File archived as ", archive_file, "\n")
            file.rename(dest_file, archive_file)
        }
    }

    # download new file
    dl_exit <- utils::download.file(url, dest_file)

    assertthat::assert_that(
        dl_exit == 0,
        msg = paste0("Download failed with exit code: ", dl_exit)
    )

    dest_file
}


#' Read Alliance .tsv.gz File
#'
#' Reads in a .tsv or .tsv.gz file from the Alliance of Genome Resources
#' as a tibble. It is recommended that Alliance files be downloaded using
#' [download_alliance_tsv()].
#'
#' @param alliance_tsv path to Alliance .tsv or .tsv.gz file
#'
#' @return
#' A dataframe.
#'
#' @export
read_alliance <- function(alliance_tsv) {

    ## identify header (to skip)
    ##  Skipping instead of using comment = "#" because data gets truncated
    ##  where values contain "#" (e.g. 'Tg(Alb-Mut)#Cpv')
    header_end <- readr::read_lines(alliance_tsv, n_max = 100) %>%
        stringr::str_detect("^#") %>%
        which() %>%
        max()

    alliance_tbl <- readr::read_tsv(
        alliance_tsv,
        skip = header_end,
        col_types = readr::cols(.default = readr::col_character())
    )

    set_alliance_tbl(alliance_tbl, version_info = alliance_tsv)
}


#' Count Alliance Records
#'
#' Counts records in data from the Alliance of Genome Resources. Counts can
#' be ascribed to the species the record is associated with or the Model
#' Organism Database (MOD) that curated it, optionally by object type.
#' `count_alliance_records()` was primarily designed to count records in the
#' Alliance Disease Associations File. _There is no guarantee that any/all_
#' _options will work for other files._
#'
#' A record is, as defined here, is the information annotated to a unique object
#' (gene, allele, model). That means for the following `record_lvl` values:
#'
#' * "doid" counts unique DOID-object annotations
#'
#' * "unique" counts full non-duplicate records
#'
#' @section NOTE:
#' For disease-related data, some exact duplicates (reason unknown) and records
#' that differ by seemingly unimportant information (e.g. only the date differs)
#' have existed. These types of duplicates are removed prior to record counts.
#'
#' @param alliance_tbl a dataframe derived from Alliance data (usually a
#' [downloaded .tsv file](https://www.alliancegenome.org/downloads))
#' @param record_lvl a string indicating the desired specificity of records;
#' one of "doid" or "unique"
#' @param by_type logical indicating whether to count by object type
#' @param pivot logical indicating whether to pivot values to type columns;
#' ignored if type = FALSE
#' @param assign_to how to assign records when counting; one of "species" or
#' "curator" (i.e. the organization responsible for curating the record)
#'
#' @return
#' A summary tibble with the count of unique object annotations defined by
#' `record_lvl`, aggregated according to species/curator (`assign_to`) and,
#' optionally, object type (`by_type`).
#'
#' @export
count_alliance_records <- function(alliance_tbl,
                                   by_type = TRUE, pivot = TRUE,
                                   record_lvl = c("doid", "unique"),
                                   assign_to = c("species", "curator")) {

    # validate arguments
    record_lvl <- match.arg(record_lvl, choices = c("doid", "unique"))
    assign_to <- match.arg(assign_to, choices = c("species", "curator"))
    assertthat::assert_that(
        rlang::is_scalar_logical(by_type),
        rlang::is_scalar_logical(pivot)
    )

    # tidy input data
    alliance_dedup <- dplyr::filter(
        alliance_tbl,
        # remove exact & date duplicates
        !duplicated(dplyr::select(alliance_tbl, -.data$Date))
    ) %>%
        # simplify object type and set desired print order
        dplyr::mutate(
            obj_type = dplyr::recode(
                .data$DBobjectType,
                "affected_genomic_model" = "model"
            ),
            obj_type = factor(
                .data$obj_type,
                levels = c("gene", "allele", "model")
            ),
            DBObjectType = NULL
        )

    if (assign_to == "curator") {
        record_df <- alliance_dedup %>%
            dplyr::mutate(
                curator = id_mod(Source),
                Source = NULL
            )
        count_by <- "curator"
    } else {
        record_df <- alliance_dedup
        count_by <- "SpeciesName"
    }

    # set columns to use for record counts
    cols_include <- switch(
        record_lvl,
        doid = c("DBObjectID", "DOID"),
        unique = names(record_df)
    )

    if (isTRUE(by_type)) {
        record_count <- record_df %>%
            dplyr::select(c(count_by, cols_include, "obj_type")) %>%
            unique()

        if (assign_to == "curator") {
            record_count <- rm_dup_curator_alliance(record_count)
        }

        record_count <- record_count %>%
            dplyr::count(
                dplyr::across(c(count_by, "obj_type")),
                name = "record_n"
            )

        if (isTRUE(pivot)) {
            record_count <- record_count %>%
                tidyr::pivot_wider(
                    names_from = .data$obj_type,
                    values_from = .data$record_n
                ) %>%
                # dplyr::select(curator, gene, allele, model) %>%
                dplyr::rename_with(
                    .fn = ~paste0(.x, "_n"),
                    .cols = -count_by
                )
        }

    } else {

        record_count <- record_df %>%
            dplyr::select(c(count_by, cols_include)) %>%
            unique()

        if (assign_to == "curator") {
            record_count <- rm_dup_curator_alliance(record_count)
        }

        record_count <- record_count %>%
            dplyr::count(
                dplyr::across(count_by),
                name = "record_n"
            )
    }

    set_alliance_tbl(record_count, version_info = alliance_tbl)
}


#' Writes Alliance Counts to File
#'
#' Writes Alliance record counts from alliance_tbl to a .csv file with the
#' version info of the Alliance file it came from as a footer.
#'
#' @param counts_tbl record counts as `alliance_tbl` (e.g. output from
#' [count_alliance_records()])
#' @param file file to write to
#' @param ... arguments to pass on to [readr::write_csv()]
#'
#' @returns
#' Returns the `counts_tbl` with its version info footer invisibly.
#' @export
save_alliance_counts <- function(counts_tbl, file, ...) {

    # prepare empty row as spacer between data and version info
    col_n <- ncol(counts_tbl)
    tmp_col_names <- paste0("c", 1:col_n)
    spacer <- matrix("", nrow = 1, ncol = col_n) %>%
        tibble::as_tibble(.name_repair = ~tmp_col_names)

    # create version info footer (with spacer)
    v <- alliance_version(counts_tbl)
    v_footer <- matrix("", nrow = 2, ncol = col_n) %>%
        tibble::as_tibble(.name_repair = ~tmp_col_names) %>%
        dplyr::mutate(
            c1 = names(v),
            c2 = unlist(v)
        ) %>%
        dplyr::add_row(!!!spacer, .before = 1) %>%
        purrr::set_names(names(counts_tbl))

    tbl_out <- counts_tbl %>%
        # convert all to character to avoid type mismatch errors
        dplyr::mutate(
            dplyr::across(
                dplyr::everything(),
                as.character
            )
        ) %>%
        # add version info footer
        dplyr::bind_rows(v_footer)

    # write converting NA to 0
    readr::write_csv(tbl_out, file = file, na = "0", ...)
}
