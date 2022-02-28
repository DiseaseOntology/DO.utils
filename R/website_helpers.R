# plot_def_src() helper
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", env=parent.frame())
    df$Source %in% c("ncbi.nlm.nih.gov", "nlm.nih.gov") &
        stringr::str_detect(df$path, subdomain)
}


# plot_citedby() helper
clean_pub_type <- function(x, as_fctr = TRUE) {
    x_lc <- stringr::str_to_lower(x)
    tidy <- dplyr::case_when(
        stringr::str_detect(x, "retract") ~ "Retracted",
        stringr::str_detect(x, "clinical trial") ~ "Clinical Trial",
        stringr::str_detect(x, "review") ~ "Review",
        stringr::str_detect(x, "conference") &
            x != "Journal|Conference Paper" ~ "Conference",
        stringr::str_detect(x, "book") ~ "Book",
        stringr::str_detect(x, "journal.*article") ~ "Article",
        TRUE ~ "Commentary"
    )

    if (as_fctr) {
        tidy <- factor(
            x,
            # order by least to most important (most important at bottom of graph)
            levels = c("Retracted", "Commentary", "Review", "Conference", "Book",
                       "Clinical Trial", "Article")
        )
    }

    tidy
}
