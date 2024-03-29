% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predicates.R
\name{obo_ID_predicates}
\alias{obo_ID_predicates}
\alias{is_valid_obo}
\alias{is_valid_doid}
\title{OBO ID predicates}
\usage{
is_valid_obo(x)

is_valid_doid(x)
}
\arguments{
\item{x}{A set of IDs, as a character vector.}
}
\description{
These predicates are designed to identify and validate common ID formats
defined within OBO Foundry ontologies.
}
\details{
\itemize{
\item \code{is_valid_obo()} to determine if identifiers match the OBO Foundry IRI
pattern or are an obo:LUI CURIE.
\item \code{is_valid_doid()} to determine if identifiers match DO's IRI or CURIE
pattern.
}
}
\section{Notes}{

These predicates \emph{do not} attempt to confirm any ID actually exists in an
OBO Foundry ontology, but only test if the IDs are syntactically formatted
correctly (see \href{https://obofoundry.org/id-policy}{OBO Foundry ID Policy} and
\href{https://owlcollab.github.io/oboformat/doc/obo-syntax.html}{OBO File Specification}).

Not all OBO formats are valid DOID formats and vice versa.
}

\examples{
# OBO formats
obo_id <- c(
    #### valid ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "<http://purl.obolibrary.org/obo/CL_0000066>", # bracketed_URI
    "obo:DOID_4", # CURIE, standard version
    "obo:so#has_origin", # '#' separator ~ OBO annotation properties
    #### invalid ####
    "0001816", # bare number without prefix
    "obo:DOID:14566", # namespace-lui separator must be '_' or '#'
    " obo:HP_0000001" # must have NO `[:space:]` characters
)

is_valid_obo(obo_id)

# DOID formats
doid <- c(
    #### valid ####
    "http://purl.obolibrary.org/obo/DOID_0001816", # URI
    "DOID:4", # CURIE, standard version
    "obo:DOID_4", # OBO CURIE, less common
    "DOID_0040001", # basename (OBO prefix removed)
    #### invalid ####
    "0001816", # bare number without prefix
    "doid#DO_IEDB_slim", # namespace-lui separator must be '_'
    "obo:doid#DO_IEDB_slim",
    "obo:DOID_21 " # must have NO `[:space:]` characters
)

is_valid_doid(doid)

}
\seealso{
Other ID predicates: 
\code{\link{is_curie}()}

Other predicates: 
\code{\link{char_val_predicates}},
\code{\link{is_curie}()},
\code{\link{is_invariant}()},
\code{\link{lgl_predicates}},
\code{\link{num_val_predicates}}
}
\concept{ID predicates}
\concept{predicates}
