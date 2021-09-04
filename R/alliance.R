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

    record_count
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

    # set version and date attributes for Alliance data
    attributes(alliance_tbl) <- c(
        attributes(alliance_tbl),
        alliance_version(alliance_tsv)
    )

    alliance_tbl
}

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
#' @param url URL to Alliance file; if not provided, will be requested at console
#' @param dest_dir path to directory where file will be saved
#'
#' @return
#' Path to saved file.
#'
#' @export
download_alliance_tsv <- function(url, dest_dir) {

    # Ask for URL if missing
    if (missing(url)) {
        url <- readline(
            prompt = "Please enter the URL of the file to be downloaded from the
            Alliance. Files can be found at
            https://www.alliancegenome.org/downloads. Right-click on the 'tsv'
            link of a desired file, select 'Copy Link'. Then paste it in this
            console and press ENTER: "
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

#' Get Alliance Version (internal)
#'
#' Gets version of Alliance of Genome Resources directly from .tsv file header.
#'
#' @param alliance_tsv Alliance .tsv data file
#' @param as_string if FALSE (default), returns Alliance version and file
#' datetime as list; if TRUE, returns Alliance version and file datetime as
#' string
#'
#' @noRd
alliance_version <- function(alliance_tsv, as_string = FALSE) {

    # validate
    assertthat::assert_that(rlang::is_scalar_logical(as_string))

    header <- readLines(alliance_tsv, n = 30)
    version_date <- grep(
        "^#.*(version|date).*:",
        header,
        ignore.case = TRUE,
        value = TRUE
    ) %>%
        stringr::str_remove("#") %>%
        stringr::str_squish() %>%
        stringr::str_split(": ")

    if (as_string) {
        vd_string <- version_date %>%
            purrr::map(2) %>%
            unlist() %>%
            vctr_to_string(delim = "_") %>%
            stringr::str_replace_all(
                c("[-:]" = "", " " = "_")
            )

        return(vd_string)
    }

    vd_list <- purrr::set_names(
        purrr::map(version_date, 2),
        nm = stringr::str_replace_all(
            purrr::map(version_date, 1),
            c(" " = "_", "[()]" = "")
        )
    )

    vd_list
}

#' Remove Curator Duplicates
#'
#' Removes Alliance records that are only unique because they are curated by a
#' MOD and the Alliance
#'
#' @noRd
rm_dup_curator_alliance <- function(df) {
    dup <- df %>%
        dplyr::select(-.data$curator) %>%
        all_duplicated()

    filter(df, !(dup & .data$curator == "Alliance"))
}
