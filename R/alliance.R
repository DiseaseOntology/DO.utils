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

    ## identify header (to skip), if any
    ##  Skipping instead of using comment = "#" because data gets truncated
    ##  where values contain "#" (e.g. 'Tg(Alb-Mut)#Cpv')
    data_start <- readr::read_lines(alliance_tsv, n_max = 100) %>%
        stringr::str_trim() %>%
        stringr::str_detect("^#|^$", negate = TRUE) %>%
        which() %>%
        min()

    if (length(data_start) == 0) {
        header_end <- 0
    } else {
        header_end <- data_start - 1
    }

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
#' The type of record information to use in counting should be specified with
#' `record_lvl` which accepts the following values:
#'
#' * "full_record" counts full non-duplicate records
#'
#' * "disease-object" counts unique disease-object combinations
#'
#' * "disease" counts unique diseases
#'
#' * "object" counts unique MOD objects (i.e. gene, allele, model identifiers)
#'
#' @section NOTE:
#' For disease-related data, some exact duplicates (reason unknown) and records
#' that differ by seemingly unimportant information (e.g. only the date differs)
#' have existed. These types of duplicates are removed prior to record counts.
#'
#' @param alliance_tbl a dataframe derived from Alliance data (usually a
#'     [downloaded .tsv file](https://www.alliancegenome.org/downloads))
#' @param term_subset character vector of DOIDs to limit counts to
#' @param by_type logical indicating whether to count by Alliance object type
#'     (i.e. gene, allele, model)
#' @param pivot logical indicating whether to pivot values to type columns;
#'     ignored if by_type = FALSE.
#' @param record_lvl a string indicating the desired specificity of records.
#' @param assign_to how to assign records when counting; one of "species" or
#'     "curator" (i.e. the organization responsible for curating the record)
#'
#' @return
#' A summary tibble with the count of unique object annotations defined by
#' `record_lvl`, aggregated according to species/curator (`assign_to`) and,
#' optionally, object type (`by_type`).
#'
#' @export
count_alliance_records <- function(alliance_tbl, term_subset = NULL,
                                   by_type = TRUE, pivot = TRUE,
                                   record_lvl = "disease-object",
                                   assign_to = c("species", "curator")) {

    # validate arguments
    assertthat::assert_that(
        is.null(term_subset) || is.character(term_subset),
        rlang::is_scalar_logical(by_type),
        rlang::is_scalar_logical(pivot)
    )
    record_lvl <- match.arg(
        record_lvl,
        choices = c("full_record", "disease-object", "disease", "object")
    )
    assign_to <- match.arg(assign_to, choices = c("species", "curator"))

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
        ) %>%
        dplyr::rename(species = .data$SpeciesName)

    if (!is.null(term_subset)) {
        alliance_dedup <- dplyr::filter(
            alliance_dedup,
            .data$DOID %in% term_subset
        )
    }

    if (assign_to == "curator") {
        record_df <- alliance_dedup %>%
            dplyr::mutate(
                curator = id_mod(.data$Source),
                Source = NULL
            )
    } else {
        record_df <- alliance_dedup
    }

    # set columns to use for record counts
    cols_include <- switch(
        record_lvl,
        full_record = names(record_df),
        "disease-object" = c("DBObjectID", "DOID"),
        disease = "DOID",
        object = "DBObjectID"
    )

    # set name of count column
    count_col_nm <- paste0(record_lvl, "_n")

    if (isTRUE(by_type)) {
        count_by <- c(assign_to, "obj_type")
    } else {
        count_by <- assign_to
    }

    record_count <- record_df %>%
        dplyr::select(c(count_by, cols_include)) %>%
        unique()

    if (assign_to == "curator") {
        record_count <- rm_dup_curator_alliance(record_count)
    }

    record_count <- record_count %>%
        dplyr::count(
            dplyr::across(count_by),
            name = count_col_nm
        )

    if (isTRUE(by_type) && isTRUE(pivot)) {
        record_count <- record_count %>%
            tidyr::pivot_wider(
                names_from = .data$obj_type,
                values_from = count_col_nm
            ) %>%
            dplyr::rename_with(
                .fn = ~paste0(.x, ".", record_lvl, "_n"),
                .cols = -assign_to
            )
    }

    set_alliance_tbl(record_count, version_info = alliance_tbl)
}


#' Writes Alliance Counts to File
#'
#' Writes Alliance record counts to a .csv file with the version info of the
#' Alliance file it came from as a footer.
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
