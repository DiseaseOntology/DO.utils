% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify.R
\name{onto_missing}
\alias{onto_missing}
\alias{identify_missing}
\title{Identify Data Missing from an Ontology (DEPRECATED)}
\usage{
onto_missing(onto_path, input, what = "OMIM", report_present = TRUE)
}
\arguments{
\item{onto_path}{The path to an ontology -edit file, as a string.}

\item{input}{The path to a TSV/CSV file with data to compare against the data
in the ontology, as a string.}

\item{what}{What to compare between the two files, as a string.}

\item{report_present}{Whether to report data present in the ontology
(default) along with data missing, as a boolean.}
}
\value{
For \code{report_present = TRUE} (default), a list with two data.frames indicating
data in the ontology (\code{in_onto}) and data missing (\code{missing}); otherwise,
a single data.frame showing the missing data only.
}
\description{
Identifies data missing from an ontology. Currently only works for mappings,
includes both xrefs and skos matches. Requires
\href{https://robot.obolibrary.org/}{ROBOT} for comparison.
}
\examples{
\dontrun{
onto_missing(
    onto_path = "~/Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
    input = "omimps.csv", # e.g. manually copied from https://www.omim.org/phenotypicSeries/PS609060
    "OMIM"
)
}
}
