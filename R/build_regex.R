build_regex <- function(x) {
    pos_df <- str_split(x, "") %>%
        purrr::map(~ purrr::set_names(.x, nm = as.character(1:length(.x)))) %>%
        tibble::enframe() %>%
        tidyr::unnest_wider(.data$value)

    invariant <- purrr::map_lgl(pos_df, is_invariant)

    df_collapsed <- purrr::map_if(
        .x = pos_df,
        .p = invariant,
        .f = unique,
        .else = ~vctr_to_string(sort(unique(.x)), delim = "")
    )

    # need to finish
    #   - identify similar neighbors --> pivot_longer, == lag?
    #   - count similar neighbors
    #   - create regex pattern

}
