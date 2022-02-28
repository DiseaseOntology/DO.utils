# plot_def_src() helper
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", env=parent.frame())
    df$Source %in% c("ncbi.nlm.nih.gov", "nlm.nih.gov") &
        stringr::str_detect(df$path, subdomain)
}


# plot_citedby() helper
clean_pub_type <- function(pub_type, as_fctr = TRUE) {
    pt_lc <- stringr::str_to_lower(pub_type)
    tidy <- dplyr::case_when(
        stringr::str_detect(pt_lc, "retract") ~ "Retracted",
        stringr::str_detect(pt_lc, "clinical trial") ~ "Clinical Trial",
        stringr::str_detect(pt_lc, "review") ~ "Review",
        stringr::str_detect(pt_lc, "conference") &
            pt_lc != "Journal|Conference Paper" ~ "Conference",
        stringr::str_detect(pt_lc, "book") ~ "Book",
        stringr::str_detect(pt_lc, "journal.*article") ~ "Article",
        TRUE ~ "Commentary"
    )

    if (as_fctr) {
        tidy <- factor(
            tidy,
            # order by least to most important (most important at bottom of graph)
            levels = c("Retracted", "Commentary", "Review", "Conference", "Book",
                       "Clinical Trial", "Article")
        )
    }

    tidy
}

# establish DO theme
theme_DO <- function(base_size = 11, base_family = "",
                     base_line_size = base_size/22,
                     base_rect_size = base_size/22) {
    half_line <- base_size/2
    theme_grey(
        base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size
    ) %+replace%
        theme(
            panel.background = element_rect(fill = "grey60", colour = NA),
            panel.grid = element_line(colour = "grey50"),
            panel.grid.major = element_line(size = rel(0.3)),
            panel.grid.minor = element_line(size = rel(0.15)),
            axis.ticks = element_line(colour = "grey40", size = rel(0.3)),
            legend.key = element_blank(),
            strip.background = element_rect(fill = "grey20", colour = NA),
            strip.text = element_text(
                colour = "grey90",
                size = rel(0.8),
                margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line)
            ),
            complete = TRUE
        )
}
