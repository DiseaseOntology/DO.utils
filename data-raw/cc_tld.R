## code to prepare `cc_tld` dataset from wikipedia table

library(tidyverse)
library(rvest)
library(janitor)

cc_tld_wiki <- "https://en.wikipedia.org/wiki/Country_code_top-level_domain"
cc_tld_html <- rvest::read_html(cc_tld_wiki)
cc_tld_tbls <- rvest::html_table(cc_tld_html) %>%
    purrr::map(
        function(tbl) {
            names(tbl) <- stringr::str_remove(
                names(tbl),
                "\\[[0-9]+\\]"
            )
            janitor::clean_names(
                tbl,
                replace = c(`'` = "", `"` = "", `%` = "_pct_", `#` = "_num_",
                            "IPv" = "ipv")
            )
        }
    )

tbl_idx <- purrr::map2(
    cc_tld_tbls,
    1:length(cc_tld_tbls),
    ~ setNames(rep(.y, length(.x)), names(.x))
) %>%
    unlist()

# generic country-code top-level domains
#   (ie. domains that the country allows outside entities to use)
gcc_tld <- cc_tld_tbls[[tbl_idx[names(tbl_idx) == "gcc_tld"]]]

# country-code top-level domains (latin characters only)
cc_tld <- cc_tld_tbls[[tbl_idx[names(tbl_idx) == "explanation"]]] %>%
    dplyr::mutate(generic = name %in% gcc_tld$gcc_tld) %>%
    dplyr::select(domain = name, entity, generic)


usethis::use_data(cc_tld, overwrite = TRUE)

usethis::use_r("data", open = TRUE)
