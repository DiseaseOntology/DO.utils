## prepare `nlm_catalog` dataset

library(tidyverse)
library(rentrez)
library(keyring)

rentrez::set_entrez_key(keyring::key_get("ENTREZ_KEY"))


# Download & save complete NLM catalog from 2009-2021 ---------------------

nlm_catalog_id <- rentrez::entrez_search(
    db = "nlmcatalog",
    term = "2009:2021[Publication Year]",
    use_history = TRUE
)

retstart_pos <- seq(1, nlm_catalog_id$count, by = 10000)
nlm_catalog_summary_list <- purrr::map(
    retstart_pos,
    function(rs) {
        tryCatch(
            rentrez::entrez_summary(
                db = "nlmcatalog",
                web_history = nlm_catalog_id$web_history,
                always_return_list = TRUE,
                retmode = "xml",
                retstart = rs,
                retmax = 10000
            ),
            error = function(e) {
                list(
                    error_msg = conditionMessage(e),
                    retstart = rs
                )
            }
        )
    }
)

nlm_catalog_complete <- purrr::map_dfr(nlm_catalog_summary_list, as_tibble)

save(
    nlm_catalog_complete,
    file = "data-raw/nlm_catalog_complete.rda",
    compress = "xz"
)


# Create titles/abbreviation catalog for use by DO.utils ------------------

nlm_catalog <- nlm_catalog_complete %>%
    tidyr::hoist(
        .col = TitleMainList,
        title = list(1L, "Title")
    ) %>%
    # reduce list to those titles with abbreviations
    dplyr::filter(!is.na(MedlineTA)) %>%
    dplyr::select(title, abbrev_title = MedlineTA) %>%
    dplyr::mutate(
        dplyr::across(dplyr::everything(), ~stringr::str_remove(.x, "\\.$"))
    )

readr::write_csv(nlm_catalog, "data-raw/nlm_catalog.csv")
