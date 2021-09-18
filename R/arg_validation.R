# Argument Validation
# Functions to shorten argument validation tests; _for internal use only_

assert_character <- function(x) {
    assertthat::assert_that(is.character(x))
}

assert_numeric <- function(x) {
    assertthat::assert_that(is.numeric(x))
}

assert_logical <- function(x) {
    assertthat::assert_that(is.logical(x))
}

assert_scalar_logical <- function(x) {
    assertthat::assert_that(rlang::is_scalar_logical(x))
}
