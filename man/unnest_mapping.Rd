% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unnest.R
\name{unnest_mapping}
\alias{unnest_mapping}
\title{Unnest Mapping}
\usage{
unnest_mapping(
  df,
  col,
  prefix = NULL,
  prefix_sep = ":",
  best_only = TRUE,
  warn_best_gt1 = FALSE,
  ...
)
}
\arguments{
\item{df}{A data.frame.}

\item{col}{The column with \code{pyobo_map()} results, as a
\link[tidyr:tidyr_tidy_select]{tidy-select specification}.}

\item{prefix}{\emph{Optional} prefix to add to namespace local unique identifiers
(LUI; e.g. 4, the LUI for "disease" in DO), as a string; preferably to
create a complete namespace ID (e.g. "DOID:4").}

\item{prefix_sep}{\emph{Optional} separator placed between \code{prefix} and
namespace LUIs, as a string. Ignored if \code{prefix = NULL}.}

\item{best_only}{Whether to return the best scoring result(s) only, as a
boolean. \code{FALSE} will return all results. \strong{NOTE:} \code{TRUE} will return
more than one result in the case of ties.}

\item{warn_best_gt1}{Whether to warn that best mapping ties exist for
a term.}

\item{...}{
  Arguments passed on to \code{\link[tidyr:unnest]{tidyr::unnest}}
  \describe{
    \item{\code{keep_empty}}{By default, you get one row of output for each element
of the list that you are unchopping/unnesting. This means that if there's a
size-0 element (like \code{NULL} or an empty data frame or vector), then that
entire row will be dropped from the output. If you want to preserve all
rows, use \code{keep_empty = TRUE} to replace size-0 elements with a single row
of missing values.}
    \item{\code{ptype}}{Optionally, a named list of column name-prototype pairs to
coerce \code{cols} to, overriding the default that will be guessed from
combining the individual values. Alternatively, a single empty ptype
can be supplied, which will be applied to all \code{cols}.}
    \item{\code{names_sep}}{If \code{NULL}, the default, the outer names will come from the
inner names. If a string, the outer names will be formed by pasting
together the outer and the inner column names, separated by \code{names_sep}.}
    \item{\code{names_repair}}{Used to check that output data frame has valid
names. Must be one of the following options:
\itemize{
\item \verb{"minimal}": no name repair or checks, beyond basic existence,
\item \verb{"unique}": make sure names are unique and not empty,
\item \verb{"check_unique}": (the default), no name repair, but check they are unique,
\item \verb{"universal}": make the names unique and syntactic
\item a function: apply custom name repair.
\item \link[tidyr]{tidyr_legacy}: use the name repair from tidyr 0.8.
\item a formula: a purrr-style anonymous function (see \code{\link[rlang:as_function]{rlang::as_function()}})
}

See \code{\link[vctrs:vec_as_names]{vctrs::vec_as_names()}} for more details on these terms and the
strategies used to enforce them.}
  }}
}
\value{
The input data frame with additional columns \code{id}, \code{term} (namespace label),
and \code{score} (mapping score as determined by GILDA). The data frame will
have additional rows if \code{best_score = FALSE} or ties for best score exist
for a term.
}
\description{
Tidies \code{\link[=pyobo_map]{pyobo_map()}} results stored in the column of a data frame in two
steps:
\enumerate{
\item Extracts mapping results from specialized \code{ScoredMatch} python objects
(as defined by GILDA).
\item Unnests the results (from list of data frames).
}
}
