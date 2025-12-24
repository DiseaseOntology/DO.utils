#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


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
#' @importFrom rlang .env
#' @importFrom rlang :=
NULL


#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble


#' @importFrom tidyr replace_na
#' @export
tidyr::replace_na


# exact copies from https://github.com/tidyverse/glue for glueV_cum()
# MIT License
# Copyright (c) 2023 glue authors

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
has_names <- function(x) {
    nms <- names(x)
    if (is.null(nms)) {
        rep(FALSE, length(x))
    } else {
        !(is.na(nms) | nms == "")
    }
}

drop_null <- function(x) {
    x[!vapply(x, is.null, logical(1))]
}
