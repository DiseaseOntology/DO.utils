% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/collapse.R
\name{collapse_to_string}
\alias{collapse_to_string}
\title{Create a String from Inputs}
\usage{
collapse_to_string(..., delim = "|", na.rm = FALSE, unique = FALSE)
}
\arguments{
\item{...}{One or more R objects (vector, list, data.frame, etc).}

\item{delim}{A delimiter to place between vector elements (default: "|").}

\item{na.rm}{A logical scalar indicating whether \code{NA} values should be
removed (default: \code{FALSE}).}

\item{unique}{Whether to include only unique values (across all inputs), as
a logical.}
}
\description{
Creates a single string from one or more inputs with each value separated by
\code{delim}.
}
\examples{
# collapses individual vectors
collapse_to_string(1:10)

# input order is preserved
collapse_to_string(1:2, letters[1:2])
collapse_to_string(data.frame(x = 1:2, y = letters[1:2]))

# factor levels are captured (instead of numeric placeholders)
collapse_to_string(factor(letters[1:2]), "c")

# unique applies across all inputs, order is determined by first appearance
collapse_to_string(c(3, 1, 2), 1:4, unique = FALSE)
collapse_to_string(c(3, 1, 2), 1:4, unique = TRUE)

}
