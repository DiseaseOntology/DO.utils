#' Count AGR Records
#'
#' Counts records in data from the Alliance of Genome Resources (AGR) by model
#' organism database (MOD) and, optionally, by object type. A record, as used
#' here, is a row in the data (after removing likely duplicates, see NOTE).
#'
#' @section NOTE:
#' For disease-related data, some exact duplicates (reason unknown) and records
#' that differ by seemingly unimportant information (e.g. only the date differs)
#' have existed. These types of duplicates are removed prior to record counts.
#'
#' @inheritParams assign_record_to_mod
#' @param by_type logical indicating whether to count by object type
#' @param pivot logical indicating whether to pivot values to type columns;
#' ignored if type = FALSE
#'
#' @return
#' A summary tibble.
#'
#' @export
count_AGR_records <- function(AGR_df, by_type = TRUE, pivot = TRUE) {

    # remove exact & date duplicates
    AGR_dedup <- dplyr::filter(
        AGR_df,
        !duplicated(dplyr::select(AGR_df, -.data$Date))
    )

    mod_assigned_df <- assign_record_to_mod(AGR_dedup)

    if (isTRUE(by_type)) {

        mod_count <- mod_assigned_df %>%
            dplyr::mutate(
                type = dplyr::recode(
                    .data$DBobjectType,
                    "affected_genomic_model" = "model"
                ),
                type = factor(.data$type, levels = c("gene", "allele", "model"))
            ) %>%
            dplyr::count(
                .data$mod_assigned, .data$type,
                name = "record_n"
            )

        if (isTRUE(pivot)) {
            mod_count <- mod_count %>%
                tidyr::pivot_wider(
                    names_from = .data$type,
                    values_from = .data$record_n
                ) %>%
                # dplyr::select(mod_assigned, gene, allele, model) %>%
                dplyr::rename_with(
                    .fn = ~paste0(.x, "_n"),
                    .cols = -.data$mod_assigned
                )
        }

    } else {

        mod_count <- dplyr::count(
            mod_assigned_df, .data$mod_assigned,
            name = "record_n"
        )

    }

    mod_count
}

#' Assign AGR Records to MODs
#'
#' Assigns Alliance of Genome Resource (AGR) records to the appropriate model
#' organism database (MOD) using a step-wise approach. AGR records may be
#' "sourced" from the "Alliance" obscuring which MOD the records were
#' originally made in.
#'
#' The step-wise approach used to assign records is as follows:
#'
#' 1. `Source` is used where it pertains to a MOD (and not the "Alliance");
#' **Note:** "OMIM Via RGD" is assigned to RGD
#'
#' 2. `DBObjectID` is used where it comes from a MOD (i.e. not "HGNC")
#'
#' 3. `WithOrthologs` is used where the ID comes from a MOD
#'
#' 4. Any remaining records are left assigned to the "Alliance" because their
#' original source can not be identified
#'
#' @return
#' The input dataframe with an additional column (`mod_assigned`) indicating the
#' assignment.
#'
#' @param AGR_df a dataframe derived from AGR data (usually a
#' [downloaded .tsv file](https://www.alliancegenome.org/downloads))
assign_record_to_mod <- function(AGR_df) {
    df_out <- dplyr::mutate(
        AGR_df,
        namespace_id = stringr::str_remove(.data$DBObjectID, ":.*"),
        ortholog_namespace_id = stringr::str_remove(.data$WithOrthologs, ":.*"),
        mod = dplyr::case_when(
            .data$Source != "Alliance" ~ .data$Source,
            .data$namespace_id != "HGNC" ~ .data$namespace_id,
            !is.na(.data$ortholog_namespace_id) ~ .data$ortholog_namespace_id,
            TRUE ~ .data$Source
        ),
        mod_assigned = id_mod(.data$mod)
    )

    df_out <- dplyr::select(
        df_out,
        -.data$namespace_id, -.data$ortholog_namespace_id, -.data$mod
    )

    df_out
}

#' Identifies Alliance MOD
#'
#' Replace Alliance MOD codes with recognizable abbreviation or name used by
#' the Alliance.
#'
#' @param x character vector of Alliance "Source" codes
#'
#' @noRd
id_mod <- function(x) {
    mod_codes <- c(
        FB = "FlyBase", MGI = "MGD", RGD = "RGD", `OMIM Via RGD` = "RGD",
        SGD = "SGD", WB = "WormBase", ZFIN = "ZFIN" #, Alliance = "Alliance"
    )

    dplyr::recode(x, !!!mod_codes)
}

#' Read AGR .tsv.gz File
#'
#' Reads in a .tsv or .tsv.gz file from the Alliance of Genome Resources (AGR)
#' as a tibble. It is recommended that AGR files be downloaded using
#' [download_AGR()].
#'
#' @param AGR_tsv path to AGR .tsv or .tsv.gz file
#'
#' @return
#' A dataframe.
#'
#' @export
read_AGR <- function(AGR_tsv) {

    ## identify header (to skip)
    ##  Skipping instead of using comment = "#" because data gets truncated
    ##  where values contain "#" (e.g. 'Tg(Alb-Mut)#Cpv')
    header_end <- readr::read_lines(AGR_tsv, n_max = 100) %>%
        stringr::str_detect("^#") %>%
        which() %>%
        max()

    AGR_df <- readr::read_tsv(
        AGR_tsv,
        skip = header_end,
        col_types = readr::cols(.default = readr::col_character())
    )

    AGR_df
}

#' Download AGR .tsv.gz File
#'
#' Downloads a URL-specified .tsv.gz file from the Alliance of Genome
#' Resources (AGR). Files can be found at
#' <https://www.alliancegenome.org/downloads>. Right-click on the "tsv" link
#' of a desired file and select "Copy Link" to get the file URL.
#'
#' A date stamp indicating download date is added to the base file name.
#'
#' @section Recommendation:
#' Although it's possbile to directly read a file from the URL, downloading it
#' promotes reproducibility and ensures future access if needed.
#'
#' @param url URL to AGR file; if not provided, will be requested at console
#' @param dest_dir path to directory where file will be saved
#'
#' @return
#' Path to saved file.
#'
#' @export
download_AGR <- function(url, dest_dir) {

    # Ask for URL if missing
    if (missing(url)) {
        url <- readline(
            prompt = "Please enter the URL of the file to be downloaded from AGR.
    Files can be found at https://www.alliancegenome.org/downloads. Right-click
    on the 'tsv' link of a desired file, select 'Copy Link'. Then paste it in
    this console and press ENTER: "
        )
    }

    # build destination file path from URL
    date_stamp <- today_datestamp()
    filename <- stringr::str_replace(
        basename(url),
        "(^.*)\\.tsv\\.gz",
        paste0("\\1-", date_stamp, ".tsv.gz")
    )
    dest_file <- file.path(dest_dir, filename)

    # download
    dl_exit <- utils::download.file(url, dest_file)

    assertthat::assert_that(
        dl_exit == 0,
        msg = paste0("Download failed with exit code: ", dl_exit)
    )

    dest_file
}

Alliance_version <- function(AGR_tsv) {

    header <- readLines(AGR_tsv, n = 30)
    version_date <- grep(
        "^#.*(version|date).*:",
        header,
        ignore.case = TRUE,
        value = TRUE
    )

    version <- vctr_to_string(
        stringr::str_replace(version_date, ".*: ([^ ]+).*", "\\1"),
        delim = "_"
    )

    version
}
