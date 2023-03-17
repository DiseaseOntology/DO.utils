# SPARQL support

The [rdflib](https://github.com/ropensci/rdflib) R package provides SPARQL queries by wrapping the [redlands](https://cran.r-project.org/web/packages/redland/index.html) package but cannot be used because it does not support FILTER NOT EXISTS statements.

Two possible alternatives that both support the complete set of SPARQL operations are [ROBOT](robot.obolibrary.org/) and python's [RDFLib](https://rdflib.readthedocs.io/en/stable/#).

- ROBOT requires installation of JDK 11 and _always_ writes output to a file.

- RDFLib requires installation of python.

**RDFLib** seems like the best option. The RStudio team has been working on making Python and R more interoperable by creating the [reticulate](https://rstudio.github.io/reticulate/) package, and it presumably can automatically download and install the needed Python packages when installing an R package (see https://rstudio.github.io/reticulate/articles/package.html).
