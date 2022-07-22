## code to update publication datasets: `DO_pubs` & `ST_pubs`
#   last updated 2022-07-22

library(tidyverse)
library(googlesheets4)

gs_id <- "1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY"
do_sheet <- "DO_pubs"
st_sheet <- "ST_pubs"

# custom function
get_pub_data <- function(gs, sheet) {
    googlesheets4::read_sheet(
        gs,
        sheet,
        col_types = "c"
    ) %>%
        dplyr::mutate(
            first_author = stringr::str_extract(Authors, "^[^,]+")
        ) %>%
        dplyr::select(
            internal_id, pmid = `PubMed ID`, pmcid, doi = DOI, scopus_eid = EID,
            lens_id, semantic_scholar_id, first_author, title = Title,
            citation_nlm
        )
}

DO_pubs <- get_pub_data(gs_id, do_sheet)
usethis::use_data(DO_pubs, overwrite = TRUE)

ST_pubs <- get_pub_data(gs_id, st_sheet)
usethis::use_data(ST_pubs, overwrite = TRUE)

usethis::use_r("data", open = TRUE)
