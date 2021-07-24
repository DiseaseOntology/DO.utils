# Test getting "cited by" info from rentrez
# Executed: 2021-07-21

library(tidyverse)
library(rentrez)
library(DO.utils)
library(tidytext)


# Identify relevant publications ------------------------------------------

do_pmid <- c(30407550, 29590633, 26093607, 25841438, 25348409, 22080554,
             19594883, 19478018)

do_citedby_pmid <- rentrez::entrez_link(
    dbfrom = "pubmed",
    id = do_pmid,
    db = "pubmed",
    cmd = "neighbor",
    # by_id = TRUE,
    linkname = "pubmed_pubmed_citedin"
)

cat("total count:", length(do_citedby_pmid$links$pubmed_pubmed_citedin))

