#' Prioritized List of Publication IDs for Matching
pub_id_types <- c("pmid", "pmcid", "doi", "scopus_eid")

#' Citation Matching
#'
#' Essentially, [base::match()] but tailored to citations. Returns a vector of
#' the positions of (first) matches of a citation input (first argument)
#' in a reference citation input (second argument) based on common
#' publication IDs (PMID, PMCID, DOI).
#'
#' The citation inputs can be vectors, data.frames, or a mix. For matching,
#' each input requires at least one vector containing one of the three standard,
#' consistent publication identifiers (PMID, PMCID, or DOI). Columns in
#' data.frames are identified by name (case-insensitive, e.g. 'PMID' or 'pmid').
#'
#' When both inputs are data.frames, `match_citations()` will match on all
#' ID types present in both inputs, returning matches from the highest priority
#' ID type (priority: PMID > PMCID > DOI > Scopus EID). It also tells the user
#' what ID columns are identified and the order used in a message.
#'
#' When no matching citation from `ref` can be found, `NA` is returned.
#'
#' @param x,ref a vector of PMID, PMCID, DOI, or Scopus IDs; or a "citation"
#' dataframe with 1 or more of these as columns (column names should correspond
#' to ID type; case-insensitive)
#' @param add_col The name of the column to add to `x` with the match results,
#'     as a string, or `NULL` (default) if results should be returned as a
#'     vector.
#' @inheritParams base::match
#'
#' @return
#' If `x` is a vector or `add_col` is FALSE (default), an integer vector of the
#' same length as `x` (if a data.frame, of length equal to the number of rows).
#' If `x` is a data.frame and `add_col` is TRUE, a mutated data.frame with a
#' `cite_match` integer column identifying match positions in `ref`.
#'
#' @export
match_citations <- function(x, ref, add_col = NULL, nomatch = NA_integer_) {

    # validate inputs
    assertthat::assert_that(
        is_vctr_or_df(x),
        is_vctr_or_df(ref),
        is.null(add_col) || rlang::is_string(add_col)
    )
    if (!is.null(add_col) & !is.data.frame(x)) {
        rlang::abort(
            message = "To add results with add_col, x must be a data.frame."
        )
    }

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

    type_both <- x_type[x_type %in% ref_type]

    # error if no matching columns
    if(length(type_both) == 0) {
        msg_header <- if (is.data.frame(x) & is.data.frame(ref)) {
            "No matching ID columns in x & ref"
        } else {
            "No IDs of same type identified"
        }
        rlang::abort(
            message = c(
                msg_header,
                paste0("One of ", vctr_to_string(pub_id_types, delim = ", "),
                       " must be present in both."
                )
            )
        )
    }

    if (length(type_both) == 1) {
        if (is.data.frame(x) && is.data.frame(ref)) {
            message("Matching by type: ", type_both)
        }

        if (is.data.frame(x)) {
            x1 <- x[[type_both]]
        } else {
            x1 <- x
        }

        if (is.data.frame(ref)) {
            ref1 <- ref[[type_both]]
        } else {
            ref1 <- ref
        }

        match_vctr <- match_carefully(x1, ref1, nomatch)
    } else {
        # for both data.frame (guaranteed if length(type_both) > 1)
        types <- priority_sort(type_both, levels = pub_id_types)

        message("Matching by types: ")

        id_matches <- purrr::map(
            .x = types,
            function(type) {
                message("* ", type)
                x_col <- get_pub_id_col(x, type)
                ref_col <- get_pub_id_col(ref, type)
                match_res <- match_carefully(x_col, ref_col, nomatch)
            }
        )

        match_vctr <- dplyr::coalesce(!!!id_matches)
    }

    if (!is.null(add_col)) {
        match_df <- dplyr::mutate(x, {{ add_col }} := match_vctr)
        return(match_df)
    }

    match_vctr
}


#' Matches Carefully
#'
#' Wrapper for [base::match()] that will NOT match `NA` values. Uses
#' [dplyr::if_else()] to skip `NA` values.
#'
#' @inheritParams base::match
#'
#' @export
#'
#' @family carefully
match_carefully <- function(x, table, nomatch = NA_integer_,
                            incomparables = NULL) {
    dplyr::if_else(
        is.na(x),
        NA_integer_,
        match(x, table, nomatch, incomparables)
    )
}


#' Fuzzy (Approximate) String Matching
#'
#' Wraps [stringdist::amatch()] to perform "fuzzy" (approximate) string
#' matching while providing more informative output. Instead of an integer
#' vector of best match positions, this function returns a tibble with the
#' input, its corresponding best match, and the approximate string distance.
#'
#' @section NOTES:
#' Fuzzy string matching is _SLOW_. Expect this function to take >1 min for
#' comparisons of more than 500 values for all methods.
#'
#' For comparison of citation titles specifically, the "lcs" method is faster
#' than "osa" and seems to work better. Based on light experimentation, a good
#' setting for `maxDist` value for citation titles is between 80-115.
#'
#' @inheritParams stringdist::amatch
#' @param ... arguments passed on to [stringdist::amatch()]
#'
#' @return
#' A tibble with 3 columns:
#' * `x`
#' * `table_match`: the closest match of `x`
#' * `dist`: the distance between x and its closest match (given the method
#' selected
#'
#' @export
match_fz <- function(x, table, method = "lcs", maxDist = 115, ...) {

    table_match_idx <- stringdist::amatch(
        x,
        table,
        method = method,
        maxDist = maxDist,
        ...
    )

    # create df of set 1 & 2 citations
    match_df <- tibble::tibble(
        x = x,
        table_match = table[table_match_idx]
    )

    match_df <- dplyr::mutate(
        match_df,
        dist = purrr::map2_dbl(
            # $ syntax to ensure match within df
            .x = match_df$x,
            .y = match_df$table_match,
            ~ stringdist::stringdist(.x, .y, method = method)
        )
    )

    match_df
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

    id_cols <- pub_id_types[pub_id_types %in% names(df_lc)]

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

    id_type <- dplyr::case_when(
        stringr::str_detect(x, "^10.+/.+$") ~ "doi",
        stringr::str_detect(x, "^PMC[0-9]+$") ~ "pmcid",
        stringr::str_detect(x, "^[0-9]{8}$") ~ "pmid",
        stringr::str_detect(x, "^2-s2.0-[0-9]{11}$") ~ "scopus_eid",
        !is.na(x) ~ "not identifiable"
    )

    id_no_type <- id_type == "not identifiable"

    assertthat::assert_that(
        !any(stats::na.omit(id_no_type)),
        msg = paste0(
            "Has ID values of unexpected type: ",
            vctr_to_string(x[id_no_type], delim = ", ")
        )
    )

    id_type <- unique(stats::na.omit(id_type))

    assertthat::assert_that(
        length(id_type) > 0,
        msg = "No ID type could be identified"
    )

    assertthat::assert_that(
        length(id_type) == 1,
        msg = paste0(
            "Mixed ID types in vector. Types identified: ",
            vctr_to_string(
                sort(id_type, na.last = TRUE),
                delim = ", "
            )
        )
    )

    assertthat::assert_that(
        id_type %in% pub_id_types
    )

    id_type
}
