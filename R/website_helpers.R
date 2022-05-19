# make_use_case_html() helper
#   sorts list into n cols (integer); fixes the alphabetical ordering from row
#   first to column first.
html_col_sort <- function(x, cols) {
    UseMethod("html_col_sort")
}

#' @export
html_col_sort.data.frame <- function(x, cols) {
    len <- nrow(x)
    idx <- create_row_index(len, cols)
    x %>%
        dplyr::mutate(.row_id = idx) %>%
        dplyr::arrange(.row_id) %>%
        dplyr::select(-.row_id)
}

#' @export
html_col_sort.default <- function(x, cols) {
    idx <- create_row_index(length(x), cols)
    purrr::map(
        1:max(idx),
        function(i) {
            x[idx == i]
        }
    ) %>%
        unlist(recursive = FALSE)
}

# Ensures correct ordering; needed because repeating 1:rows when there
# are < cols - 1 items in the last row causes misordering of last row.
create_row_index <- function(len, cols) {
    in_last_row <- len %% cols
    rows <- round_up(len / cols, 0)
    row_idx <- c(
        rep(1:rows, times = in_last_row),
        rep(1:(rows - 1), times = cols - in_last_row)
    )
    row_idx
}

# plot_def_src() helper
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", envir = parent.frame())
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
    ggplot2::`%+replace%`(
        ggplot2::theme_grey(
            base_size = base_size, base_family = base_family,
            base_line_size = base_line_size, base_rect_size = base_rect_size
        ),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "grey60", colour = NA),
            panel.grid = ggplot2::element_line(colour = "grey50"),
            panel.grid.major = ggplot2::element_line(size = ggplot2::rel(0.3)),
            panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.15)),
            axis.ticks = ggplot2::element_line(
                colour = "grey40",
                size = ggplot2::rel(0.3)
            ),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(
                fill = "grey20",
                colour = NA
            ),
            strip.text = ggplot2::element_text(
                colour = "grey90",
                size = ggplot2::rel(0.8),
                margin = ggplot2::margin(
                    0.8 * half_line,
                    0.8 * half_line,
                    0.8 * half_line,
                    0.8 * half_line
                )
            ),
            complete = TRUE
        )
    )
}
