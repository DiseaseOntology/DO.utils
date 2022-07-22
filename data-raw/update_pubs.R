## code to prepare `DO_pubs` dataset, last updated 2022-02-15

library(tidyverse)
library(googlesheets4)

gs_id <- "1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY"
gs_sheet <- "DO_pubs"

DO_pubs <- googlesheets4::read_sheet(
    gs_id,
    gs_sheet,
    col_types = "c"
) %>%
    dplyr::mutate(
        first_author = stringr::str_extract(Authors, "^[^,]+")
    ) %>%
    dplyr::select(
        internal_id, pmid = `PubMed ID`, pmcid, doi = DOI, scopus_eid = EID,
        lens_id, semantic_scholar_id, first_author, title = Title, citation_nlm
    )

usethis::use_data(DO_pubs, overwrite = TRUE)
usethis::use_r("data", open = TRUE)
