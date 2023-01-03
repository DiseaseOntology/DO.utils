#
merge_citations_ <- function(df1, df2) {
    certify_unique_citations(df1)
    certify_unique_citations(df2)

    df1 <- dplyr::mutate(unique(df1), match_index = dplyr::row_number())
    df2 <- match_citations(df2, df1, add_col = "match_index")

    merged <- dplyr::bind_rows(df1, df2) %>%
        collapse_col(match_index) %>%
        dplyr::select(-match_index)

    merged
}


# merge_citations() helpers -----------------------------------------------

certify_unique_citations <- function(citation_df) {
    df_nm <- as.character(substitute(citation_df))

    id_present <- find_pub_id_cols(citation_df)
    citation_id <- dplyr::select(citation_df, id_present)

    row_na <- apply(citation_id, 1, function(x) { all(is.na(x)) })
    if (any(row_na)) {
        rlang::abort(
            c(
                paste0(
                    "All rows in `", df_nm,
                    "` must contain at least one identifier."
                ),
                x = paste0("Failing rows: ", to_range(which(row_na)))
            ),
            class = "citation_df-row_na",
            .frame = parent.frame()
        )
    }

    row_dup <- all_duplicated(citation_id)
    if (any(row_dup)) {
        rlang::abort(
            c(
                paste0("Rows in `", df_nm, "` must be unique."),
                x = paste0("Failing rows: ", to_range(which(row_dup)))
            ),
            class = "citation_df-row_dup",
            .frame = parent.frame()
        )
    }

    dup_id_list <- purrr::map(
        citation_id,
        all_duplicated,
        incomparables = NA
    ) %>%
        purrr::set_names(names(citation_id))

    if (any(unlist(dup_id_list), na.rm = TRUE)) {
        dup_id_rng <- purrr::map(dup_id_list, ~ to_range(which(.x))) %>%
            unlist() %>%
            na.omit()
        rlang::abort(
            c(
                paste0(
                    "All identifiers in `", df_nm, "` must be unique."
                ),
                purrr::set_names(
                    paste0(names(dup_id_rng), ": ", dup_id_rng),
                    nm = rep("x", length(dup_id_rng))
                )
            ),
            class = "citation_df-id_dup",
            .frame = parent.frame()
        )
    }

    invisible(TRUE)
}
