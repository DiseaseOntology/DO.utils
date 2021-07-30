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

#' Citation Matching
#'
#' Returns a vector of the positions of (first) matches of a citations
#' dataframe (first argument) in a reference citations dataframe (second
#' argument); essentially, [base::match()] but tailored to citations.
#'
#' The "citations" dataframe should consist of one or more column(s) containing
#' one type of the three standard, consistent publication identifiers (PMID,
#' PMCID, or DOI) with columns named accordingly. The same identifier columns
#' should exist in both input dataframes. These identifiers are not
#' always available for every publication; sometimes only one exists and rarely
#' none. Where more than one are provided they each will be used to mat
#'
#' @param df,ref_df a vector of PMID, PMCID, or DOI IDs; or a "citation"
#' dataframe with 1 or more of these as columns (column names should correspond
#' to ID type; case-insensitive)
#' @param add_col logical; if FALSE (default), returns a vector; if TRUE,
#' [dplyr::mutate()]s the dataframe by adding a `cite_match` column
#' @inheritParams base::match
#'
#' @return
#' If `x` is a vector or `add_col` is FALSE (default), an integer vector of the
#' same length as `x` (if a dataframe, of length equal to the number of rows).
#' If `x` is a dataframe and `add_col` is TRUE, a mutated dataframe with a
#' `cite_match` integer column identifying matches added.
#'
#' @export
match_citations <- function(x, ref, add_col = FALSE, nomatch = NA_integer_) {

    # validate inputs
    assertthat::assert_that(
        is_vctr_or_df(x),
        is_vctr_or_df(ref),
        is.logical(add_col)
    )

    if (is.vector(x)) {
        x_type <- type_pub_id(x)
    } else {
        x_type <- find_pub_id_cols(x)
    }

    if (is.vector(ref)) {
        ref_type <- type_pub_id(ref)
    } else {
        ref_type <- find_pub_id_cols(ref)
    }

    if (is.vector(x) && is.vector(ref)) {
        assertthat::assert_that(
            identical(x_type, ref_type)
        )

        match_idx <- match_carefully(x, ref, nomatch)

        return(match_idx)
    }

    type_both <- x_type[x_type %in% ref_type]

    assertthat::assert_that(
        length(type_both) > 0
    )

    if (length(type_both) == 1) {
        if (is.data.frame(x)) {
            x <- x[[type_both]]
        }

        if (is.data.frame(ref)) {
            ref <- ref[[type_both]]
        }

        match_idx <- match_carefully(x, ref, nomatch)

        return(match_idx)
    }

    # for both data.frame (guaranteed if length(type_both) > 1)
    types <- priority_sort(type_both, levels = pub_id_types())

    message("Matching by types: ", vctr_to_string(types, delim = ", "))

    id_matches <- purrr::map(
        .x = types,
        function(type) {
            x_col <- get_pub_id_col(x, type)
            ref_col <- get_pub_id_col(ref, type)
            match_res <- match_carefully(x_col, ref_col, nomatch)
        }
    )

    match_vctr <- dplyr::coalesce(!!!id_matches)

    if (is.data.frame(x) && isTRUE(add_col)) {
        match_df <- dplyr::mutate(x, cite_match = match_vctr)
        return(match_df)
    }

    match_vctr
}


#' Get Publication ID Column
#'
#' Gets a publication ID column from a data.frame and ensures the data matches
#' the specified type
#' @param df a data.frame
#' @param type the expected publication ID type (derived from the column name)
#'
#' @noRd
get_pub_id_col <- function(df, type) {

    x <- df[[type]]

    # confirm type
    computed_type <- type_pub_id(x)

    assertthat::assert_that(
        identical(type, computed_type),
        msg = paste0(
            "type (", type, ") not identical to computed type: ", computed_type
        )
    )

    x
}


#' Identifies Publication ID Columns
#'
#' Identifies columns in a data.frame with publication IDs BY NAME
#' (case-insensitive).
#'
#' @section NOTE:
#' Identifying publication ID columns by typing all columns in a data.frame
#' might possibly be safe for PMCIDs or DOIs but would not be safe for PMIDs
#' (underlying regex matches _any_ integer). Therefore, columns are identified
#' by name.
#'
#' @param df a data.frame
#'
#' @return
#' A character vector of publication ID column names.
#'
#' @noRd
find_pub_id_cols <- function(df) {
    df_lc <- dplyr::rename_with(df, tolower)

    id_cols <- pub_id_types()[pub_id_types() %in% names(df_lc)]

    assertthat::assert_that(
        length(id_cols) > 0,
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
