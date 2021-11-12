## code to prepare `nlm_catalog` dataset goes here

library(tidyverse)
library(rentrez)
library(keyring)

rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))

nlm_catalog_id <- rentrez::entrez_search(
    db = "nlmcatalog",
    term = "currentlyindexed",
    use_history = TRUE
)

nlm_catalog_summary <- rentrez::entrez_summary(
    db = "nlmcatalog",
    web_history = nlm_catalog_id$web_history,
    always_return_list = TRUE,
    retmode = "xml"
)

nlm_catalog_complete <- as_tibble(nlm_catalog_summary) %>%
    dplyr::mutate(
        dplyr::across(where(rlang::is_list), confine_list)
    )

readr::write_csv(nlm_catalog_complete, "data-raw/nlm_catalog-complete.csv")

nlm_catalog <- nlm_catalog_complete %>%
    dplyr::mutate(TitleMainList = release_list(TitleMainList)) %>%
    tidyr::hoist(
        .col = TitleMainList,
        Title = list(1L, "Title")
    ) %>%
    dplyr::select(title = Title, abbrev_title = MedlineTA) %>%
    dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_remove(.x, "\\.$"))
    )

readr::write_csv(nlm_catalog, "data-raw/nlm_catalog.csv")
