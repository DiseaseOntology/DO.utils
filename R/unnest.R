#' Unnest data frame list columns
#'
#' Like [tidyr::unnest()] but always produces a cartesian product and does not
#' requires that list columns be "parallel entries ... of compatible sizes".
#'
#' @param data A data.frame.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> Columns to unnest.
#' @inheritDotParams tidyr::unchop
#' @inheritDotParams tidyr::unpack
#'
#' @examples
#' df <- tibble::tibble(
#'     n = 1:2,
#'     interjection = list(c("oh", "wow"), "zap"),
#'     letter = list(c("a", "b", "c"), "d"),
#'     keep_list = list("please", c("don't", "unnest"))
#' )
#' df
#'
#' # Uses tidyselect semantics, like tidyr::unnest()
#' unnest_cross(df, cols = tidyselect::starts_with("inter"))
#'
#' # Works when list column sizes differ, unlike tidyr::unnest()
#' tryCatch(
#'     tidyr::unnest(df, cols = c(interjection, letter)),
#'     error = function(e) message(e)
#' )
#' unnest_cross(df, cols = c(interjection, letter))
#'
#' # Always produces a cartesian product, unlike tidyr::unnest()
#' df2 <- tibble::tibble(
#'     n = list(1:2, 3L),
#'     letter = list(c("a", "b"), "c"),
#' )
#' df2
#' tidyr::unnest(df2, cols = tidyselect::everything())
#' unnest_cross(df2, cols = tidyselect::everything())
#'
#' @export
unnest_cross <- function(data, cols, ...) {
    .df_out <- data
    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    purrr::walk(
        .cols,
        function(col) {
            .df_out <<- tidyr::unnest(.df_out, {{ col }}, ...)
        }
    )
    .df_out
}


#' Unnest Mapping
#'
#' Tidies [pyobo_map()] results stored in the column of a data frame in two
#' steps:
#' 1. Extracts mapping results from specialized `ScoredMatch` python objects
#' (as defined by GILDA).
#' 2. Unnests the results (from list of data frames).
#'
#' @param df A data.frame.
#' @param col The column with `pyobo_map()` results, as a
#'     [tidy-select specification](tidyr::tidyr_tidy_select).
#' @inheritParams parse_term_mapping
#' @inheritParams extract_ScoredMatch
#' @inheritDotParams tidyr::unnest keep_empty:names_repair
#'
#' @returns
#' The input data frame with additional columns `id`, `term` (namespace label),
#' and `score` (mapping score as determined by GILDA). The data frame will
#' have additional rows if `best_score = FALSE` or ties for best score exist
#' for a term.
#'
#' @export
unnest_mapping <- function(df, col, prefix = NULL, prefix_sep = ":",
                           best_only = TRUE, warn_best_gt1 = FALSE, ...) {

    df_unnested <- df %>%
        dplyr::mutate(
            parsed_mapping = parse_mapping(
                {{ col }},
                prefix = prefix,
                prefix_sep = prefix_sep,
                best_only = best_only,
                warn_best_gt1 = warn_best_gt1
            )
        ) %>%
        tidyr::unnest(.data$parsed_mapping, ...) %>%
        dplyr::select(- {{ col }} )

    df_unnested
}
