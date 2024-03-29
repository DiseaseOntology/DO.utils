% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format.R
\name{format_subtree}
\alias{format_subtree}
\title{Format a Subtree}
\usage{
format_subtree(subtree_df, top_node)
}
\arguments{
\item{subtree_df}{A dataframe from \code{\link[=extract_subtree]{extract_subtree()}}.}

\item{top_node}{The top node of the tree, as a valid DOID (see
\code{\link[=is_valid_doid]{is_valid_doid()}} for valid input formats).}
}
\description{
Format a subtree, produced by \code{\link[=extract_subtree]{extract_subtree()}}, as a text-based tree
mirroring \href{https://disease-ontology.org/}{disease-ontology.org}.
}
\examples{
\dontrun{
    do_owl <- {path_to_doid.owl_here}
    subtree <- extract_subtree(do_owl, "DOID:3070")
    st_formatted <- format_subtree(subtree, "DOID:3070")
    st_formatted
}

}
\seealso{
\code{\link[=extract_subtree]{extract_subtree()}}
}
