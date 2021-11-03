alliance_class <- c("alliance_tbl", tibble:::tibble_class)


#' Set alliance_tbl
#'
#' Set attributes to create an Alliance tbl
#'
#' @param df data.frame or tibble to convert to alliance_tbl
#' @param version_info where to get version and file datetime info
#'
#' @noRd
set_alliance_tbl <- function(df, version_info) {

    attributes(df) <- c(
        attributes(df),
        alliance_version(version_info)
    )

    class(df) <- alliance_class

    df
}


#' Get Alliance Version
#'
#' Gets version of Alliance of Genome Resources directly from .tsv file header.
#'
#' @param alliance_obj an Alliance .tsv data file OR an Alliance (count) tibble
#' @param as_string if FALSE (default), returns Alliance version and file
#' datetime as list; if TRUE, returns Alliance version and file datetime as
#' string
#'
#' @noRd
alliance_version <- function(alliance_obj, as_string = FALSE) {
    UseMethod("alliance_version")
}

#' @export
alliance_version.alliance_tbl <- function(alliance_obj, as_string = FALSE) {

    # validate
    assertthat::assert_that(rlang::is_scalar_logical(as_string))

    v <- attributes(alliance_obj)[c("Alliance_Database_Version", "Date_file_generated_UTC")]

    if (as_string) {
        v <- v %>%
            unlist() %>%
            vctr_to_string(delim = "_") %>%
            stringr::str_replace_all(
                c("[-:]" = "", " " = "_")
            )
    }

    v
}

#' @export
alliance_version.default <- function(alliance_obj, as_string = FALSE) {

    # validate
    assertthat::assert_that(rlang::is_scalar_logical(as_string))

    header <- readLines(alliance_obj, n = 30)
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

    dplyr::filter(df, !(dup & .data$curator == "Alliance"))
}
