% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partition.R
\name{chunk}
\alias{chunk}
\alias{partition}
\title{Partition vectors}
\usage{
partition(x, n)
}
\arguments{
\item{x}{A vector.}

\item{n}{An integer specifying instances per partition.}
}
\value{
A list with \code{x/n} items (rounded up) each containing \code{n} elements from \code{x}.
When \code{x \%\% n != 0} the last list item will have < \code{n} elements from \code{x}.
}
\description{
\code{partition} divides vectors into partitions of a specified length and returns
them as a list. If \code{x} is not completely divisible by \code{n} the last list item
will have less than \code{n} elements. Similar functions may be named "chunk".
}
\examples{
partition(letters[1:10], 5)
partition(1:10, 3)

}
