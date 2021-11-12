## code to prepare `nlm_catalog` dataset goes here

library(tidyverse)

# Dump of PubMed and NCBI molecular biology database journals from NLM Catalog
# at https://pubmed.ncbi.nlm.nih.gov/help/#journal-lists
nlm_catalog_txt <- readr::read_lines(
    "https://ftp.ncbi.nih.gov/pubmed/J_Entrez.gz"
)

extract_field <- function(x, field) {
    x[stringr::str_detect(x, field)] %>%
        stringr::str_remove(paste0(field, ":")) %>%
        stringr::str_trim()
}

nlm_catalog_complete <- tibble::tibble(
    iso_abbr = extract_field(nlm_catalog_txt, "IsoAbbr"),
    issn_online = extract_field(nlm_catalog_txt, "ISSN \\(Online\\)"),
    issn_print = extract_field(nlm_catalog_txt, "ISSN \\(Print\\)"),
    title = extract_field(nlm_catalog_txt, "JournalTitle"),
    id = extract_field(nlm_catalog_txt, "JrId"),
    med_abbr = extract_field(nlm_catalog_txt, "MedAbbr"),
    nlm_id = extract_field(nlm_catalog_txt, "NlmId")
) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), replace_blank))

readr::write_csv(nlm_catalog_complete, "data-raw/nlm_catalog-complete.csv")

# iso_abbr == med_abbr
nlm_catalog <- nlm_catalog_complete %>%
    dplyr::select(title, abbrev_title = med_abbr)

readr::write_csv(nlm_catalog, "data-raw/nlm_catalog.csv")
