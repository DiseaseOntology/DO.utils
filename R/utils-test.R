#' Simplify Examining Test Snapshots (INTERNAL)
#'
#' Formats snapshots to make manual examination of results easier.
#'
#' @param fn_name The name of a function with snapshots to examine, as a string.
#'
#' @noRd
examine_snapshots <- function(fn_name) {
    stopifnot(rlang::is_string(fn_name))
    fn <- switch(
        fn_name,
        format_axiom = examine_format_axiom_snapshots
    )
    fn()
}

#' DO NOT CALL DIRECTLY
#'
#' Produces df with:
#' * `id`: which axiom
#' * `cmd`: "original axiom" or cmd that produced result
#' * `res`: original axiom or result
#' * `paren_correct`: whether the correct number of parentheses are in a result
#' * `paren`: number of opening parentheses in result, in case paren != correct
#'
#' @noRd
examine_format_axiom_snapshots <- function(test_dir = "tests/testthat/") {

    ax_df <- tibble::tibble(
        id = 1:8,
        cmd = "original axiom",
        res = readr::read_lines(file.path(test_dir, "data/axioms.ofn")),
        paren = stringr::str_count(.data$res, "\\(")
    )

    ss <- readr::read_lines(file.path(test_dir, "_snaps/format.md"))
    code_pos <- which(stringr::str_detect(ss, "Code")) + 1
    res_pos <- which(stringr::str_detect(ss, "\\[[0-9]+\\]"))

    ss_df <- tibble::tibble(
        id = rep(1:8, 4),
        cmd = stringr::str_squish(rep(ss[code_pos], each = 8)),
        res = stringr::str_replace(ss[res_pos], '.*"(.+)".*', "\\1"),
        paren = stringr::str_count(res, "\\(")
    )

    compare_df <- dplyr::bind_rows(ax_df, ss_df) %>%
        dplyr::arrange(.data$id, dplyr::desc(.data$cmd)) %>%
        dplyr::group_by(.data$id) %>%
        dplyr::mutate(
            paren_correct = dplyr::if_else(
                .data$cmd == "original axiom",
                NA,
                .data$paren[.data$cmd == "original axiom"] - .data$paren == 1
            )
        ) %>%
        dplyr::ungroup()

    compare_df
}

