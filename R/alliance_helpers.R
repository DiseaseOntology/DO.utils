alliance_class <- c("alliance_tbl", tibble:::tibble_class)

#' Set alliance_tbl
#'
#' Set attributes to create an Alliance tbl
#'
#' @param data data.frame or tibble to convert to alliance_tbl
#' @param version_info where to get version and file datetime info
#'
#' @noRd
set_alliance_tbl <- function(data, version_info) {

    attributes(data) <- c(
        attributes(data),
        alliance_version(version_info)
    )

    class(data) <- alliance_class

    data
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

    filter(df, !(dup & .data$curator == "Alliance"))
}
