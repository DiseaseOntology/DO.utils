## code to prepare `glioma_subtree.csv` dataset (& simplified version for testing)

library(DO.utils)
library(dplyr)
library(readr)

# extract high grade glioma subtree
repo <- pyDOID$DOrepo("~/Documents/Ontologies/HumanDiseaseOntology")
df <- extract_subtree(repo$doid, top_node = "DOID:3070")

# save for examples
readr::write_csv(df, "inst/extdata/glioma_subtree.csv")

# simplify & save for testing
include <- c("DOID:3070", "DOID:5074", "DOID:5503", "DOID:7788")
test_df <- dplyr::filter(df, id %in% include)

readr::write_csv(test_df, "tests/testthat/data/format_subtree.csv")
