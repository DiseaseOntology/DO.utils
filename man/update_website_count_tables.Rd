% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/website.R
\name{update_website_count_tables}
\alias{update_website_count_tables}
\title{Update Counts in DO Website HTML}
\usage{
update_website_count_tables(DO_repo, tag, svn_repo)
}
\arguments{
\item{DO_repo}{A \code{pyDOID.repo.DOrepo} object (see \code{\link[=DOrepo]{DOrepo()}}).}

\item{tag}{The repo tag to extract data from, as a string.}

\item{svn_repo}{The local path to the DO website svn directory, as a string.
The correct directory will include a Dockerfile and the
'disease_ontology' directory.}
}
\value{
Updated counts directly in the html of the svn repo for each page,
\emph{as well as}, the old and new counts for comparison as a list of tibbles
(invisibly).
}
\description{
Directly updates counts listed in the tables on the disease-ontology.org
"DO Imports" and "DO Slims" pages using data from a specified release.
Changes to these html files should be reviewed and, if correct, committed to
the svn repo for deployment.
}
\section{Speed Note}{

Expect this function to make more than a minute. The majority of the time is
consumed loading the doid-merged.owl file and cannot be sped up
without a faster RDF/OWL parser. This is currently handled via DO.utils'
dependency on the python package pyDOID with RDF handled by the RDFLib python
package.
}

