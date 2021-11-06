# get data frame of OBO Foundry ontologies
# J. Allen Baron
# Last executed: 2021-11-05

library(jsonlite)
library(tidyverse)
library(janitor)


# define helper function to unnest data frame columns (unnest_wider can't
#   handle these currently)
# NOTE: Only works when each row is a 1 row data frame.
unnest_wider_df <- function(.df, .col, names_sep = "_") {
    col_name <- tryCatch(
        .col,
        error = function(e) {
            rlang::as_label(rlang::enquo(.col))
        }
    )

    .df[[col_name]] %>%
        dplyr::bind_rows() %>%
        tibble::as_tibble() %>%
        dplyr::rename_with( ~ paste(col_name, .x, sep = names_sep) )
}

# define helper function to fill in 1-sided logical vectors
fill_logical <- function(x) {
    # get unique value(s)
    uniq <- unique(na.omit(x))

    # if not 1-sided (contains only TRUE or only FALSE), do NOT modify
    if (length(uniq) != 1) {
        return(x)
    }
    tidyr::replace_na(x, !uniq)
}


# source of data: http://www.obofoundry.org/registry/ontologies.jsonld
obofoundry_data <- jsonlite::read_json(
    "http://www.obofoundry.org/registry/ontologies.jsonld",
    simplifyVector = TRUE
)


# tidy somewhat & save complete data set as csv (list columns as json)
obofoundry_df <- obofoundry_data$ontologies %>%
    tibble::as_tibble() %>%
    janitor::clean_names()

obofoundry_df <- purrr::map_dfc(
        names(obofoundry_df)[purrr::map_lgl(obofoundry_df,  is.data.frame)],
        ~ unnest_wider_df(obofoundry_df, .x)
    ) %>%
    dplyr::bind_cols(
        dplyr::select(obofoundry_df, !tidyselect:::where(is.data.frame))
    )

obofoundry_df <- dplyr::bind_cols(
    dplyr::select(obofoundry_df, -review_document),
    unnest_wider_df(obofoundry_df, review_document)
) %>%
    dplyr::mutate(
        dplyr::across(where(purrr::is_list), confine_list),
        dplyr::across(
            where(is.logical),
            fill_logical
        )
    )

readr::write_csv(obofoundry_df, "data-raw/obofoundry_metadata.csv")


# export meaningful data
obofoundry_metadata <- obofoundry_df %>%
    # making rename explicit here
    dplyr::rename(contact_name = contact_label) %>%
    dplyr::select(
        id, title, domain, description, activity_status, is_obsolete,
        replaced_by, ontology_purl, preferred_prefix, homepage, contact_name,
        contact_email, contact_github, license_label, taxon_id, taxon_label,
        twitter, facebook, publications, in_foundry_order, in_foundry,
        build_infallible
    ) %>%
    dplyr::mutate(
        publications = purrr::map(release_list(publications), tibble::as_tibble)
    )
usethis::use_data(obofoundry_metadata, overwrite = TRUE)
