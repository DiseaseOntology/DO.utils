#' Identify Citations of Closest Match
#'
#' From two sets of citations, identifies the closes match for set 1 in set 2
#' below a threshold (`maxDist`) using fuzzy string matching.
#'
#' @section NOTES:
#' Fuzzy string matching is _SLOW_. Expect this function to take >1 min for
#' comparisons of more than 500 citations for all methods.
#'
#' "lcs" method is faster than "osa" and seems to work better.
#'
#'
#' @param x citations to find matches for
#' @param ref citations to find matches from
#' @inheritParams stringdist::amatch
#' @param ... arguments passed on to [stringdist::amatch()]
#'
#' @return A tibble with x, the reference most closely matching x, and the
#' distance between x and the reference match (based on the selected method);
#' columns = "x", "ref_match", "dist".
#'
#' @export
match_citations_fz <- function(x, ref, method = "lcs", maxDist = 115, ...) {

    ref_match_idx <- stringdist::amatch(
        x,
        ref,
        method = method,
        maxDist = maxDist,
        ...
    )

    # create df of set 1 & 2 citations
    match_df <- tibble::tibble(
        x = x,
        ref_match = ref[ref_match_idx]
    )

    match_df <- dplyr::mutate(
        match_df,
        dist = purrr::map2_dbl(
            # $ syntax to ensure match within df
            .x = match_df$x,
            .y = match_df$ref_match,
            ~ stringdist::stringdist(.x, .y, method = method)
        )
    )

    match_df
}

#' Identifies Publication ID Columns
#'
#' Identifies columns in a data.frame with publication IDs BY NAME
#' (case-insensitive).
#'
#' @param df a data.frame
#'
#' @return
#' A character vector of publication ID column names.
#'
#' @noRd
find_pub_id_cols <- function(df) {
    df_lc <- dplyr::rename_with(tolower)

    id_cols <- pub_id_types() %in% names(df_lc)

    assertthat::assert_that(
        length(id_cols > 0),
        msg = "No publication ID columns could be identified. At least one
        column must be named 'pmid', 'pmcid', or 'doi'"
    )

    id_cols
}


#' Identify Publication ID Type
#'
#' Returns one of pmid, pmcid, doi based on standards
#'
#' @param x a vector of same-type IDs
#' @noRd
type_pub_id <- function(x) {

    id_type <- case_when(
        stringr::str_detect(x, "^10.+/.+$") ~ "doi",
        stringr::str_detect(x, "^PMC[0-9]+$") ~ "pmcid",
        stringr::str_detect(x, "^[0-9]+$") ~ "pmid",
        !is.na(x) ~ "not identifiable"
    )

    id_no_type <- id_type == "not identifiable"

    assertthat::assert_that(
        !any(na.omit(id_no_type)),
        msg = paste0(
            "The following IDs could not be identified: ",
            vctr_to_string(x[id_no_type], delim = ", ")
        )
    )

    id_type <- unique(na.omit(id_type))
    assertthat::assert_that(
        length(id_type) > 0,
        msg = "No ID type could be identified"
    )

    assertthat::assert_that(
        length(id_type) == 1,
        msg = paste0(
            "All IDs must be of the same type. Types identified: ",
            vctr_to_string(
                sort(id_type, na.last = TRUE),
                delim = ", "
            )
        )
    )

    assertthat::assert_that(
        id_type %in% pub_id_types()
    )

    id_type
}


#' Returns List of Publication IDs
#'
#' @noRd
pub_id_types <- function() {
    c("pmid", "pmcid", "doi")
}
