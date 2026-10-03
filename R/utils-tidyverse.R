#' rlang imports (INTERNAL)
#'
#' Import from rlang:
#'
#' * [rlang::.data] pronoun to prevent verbose CMD check due to
#' non-standard evaluation.
#'
#' * [rlang::`:=`](rlang::`glue-operators`) for programmatic creation +
#' assignment of variables in the tidyverse (primarily [dplyr::mutate()]). May
#' be removed see rlang issue
#' [#1296](https://github.com/r-lib/rlang/issues/1296).
#'
#' @name rlang_imports
#' @noRd
#' @importFrom rlang .data
#' @importFrom rlang :=
NULL


#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble


#' @importFrom tidyr replace_na
#' @export
tidyr::replace_na
