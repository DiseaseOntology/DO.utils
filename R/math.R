#' Round a number up
#'
#' @details
#' Rounding to a negative number of digits means rounding to a power of ten, so
#' for example round_up(x, digits = -2) rounds to the nearest hundred.
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places. Negative
#' values are allowed (see 'Details').
#'
#' @export
round_up <- function(x, digits = 0) {

    if (digits == 0) {
        return(ceiling(x))
    }

    place <- 10^(-digits)
    x + place - x %% place
}


#' Round a number down
#'
#' @details
#' Rounding to a negative number of digits means rounding to a power of ten, so
#' for example round_down(x, digits = -2) rounds to the nearest hundred.
#'
#' @param x a numeric vector.
#' @param digits integer indicating the number of decimal places. Negative
#' values are allowed (see 'Details').
#'
#' @export
round_down <- function(x, digits = 0) {

    if (digits == 0) {
        return(floor(x))
    }

    place <- 10^(-digits)
    x - x %% place
}

#' Round Number to Zero
#'
#' @details
#' Rounding to a negative number of digits means rounding to a power of ten, so
#' for example round_zero(x, digits = -2) rounds to the nearest hundred.
#'
#' @param x A numeric vector.
#' @param digits The number of decimal places, as an integer. Negative
#' values are allowed (see 'Details').
#'
#' @export
round_zero <- function(x, digits) {
    ifelse(
        sign(x) > 0,
        round_down(x, digits = digits),
        round_up(x, digits = digits)
    )
}
