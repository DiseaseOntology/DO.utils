#' Make HTML for DO User List
#'
#' Makes the row and cell html code for the "Users of the Disease Ontology"
#' section of the collaborators page on disease-ontology.org from the DO team's
#' "Uses" google sheet. This function explicitly avoids including the html
#' code for defining the table itself to provide for flexibility.
#'
#' @param file The file path where the output should be saved, as a string.
#'
#' @export
make_user_list_html <- function(file) {
    # get data
    user_list <- googlesheets4::read_sheet(
        ss = .DO_gs$users,
        sheet = "DO_website_user_list",
        range = "A:E",
        col_types = "lcccc"
    )
    ws_user_list <- user_list %>%
        dplyr::filter(!is.na(added)) %>%
        # ensure list is alphabetical
        dplyr::arrange(name)

    # build html
    user_html <- glue::glue_data(
        .x = ws_user_list,
        '<td class="default"><a href="{url}" target="_blank">{name}</a></td>'
    )
    html_rows <- html_in_rows(user_html, per_row = 3, tab_indent = 2)

    readr::write_lines(html_rows, file = file)
}



# Statistics Plots --------------------------------------------------------

#' Plot Publications Citing DO by Year
#'
#' Plots the count of publications that cite the Human Disease Ontology by
#' year.
#'
#' @param data_file The path to the file containing the list of publications
#'     citing the DO, as a string.
#' @param out_dir The directory where the plot `"{date}-DO_cited_by_count.png"`
#'     should be saved, as a string.
#' @param w The width of the plot in inches, as an integer.
#' @param h The height of the plot in inches, as an integer.
#'
#' @section Data Preparation:
#' To prepare data, execute `scripts/citedby_full_procedure.R`.
#'
#' @export
plot_citedby <- function(data_file = "data/citedby/DO_citedby.csv",
                         out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_cited_by_count.png"
        )
    )

    df <- readr::read_csv(data_file) %>%
        dplyr::mutate(Year = lubridate::year(pub_date)) %>%
        dplyr::count(Year, name = "Publications")

    g <- ggplot(data = df) +
        geom_col(
            aes(x = Year, y = Publications),
            width = 0.6,
            fill = DO_colors["light"]
        ) +
        labs(title = "Publications Citing DO", x = "Year", y = "Count") +
        theme_dark(base_size = 13)

    ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot DO Term & Definition Counts
#'
#' Plots the count of _non-obsolete_ terms and definitions in the Human Disease
#' Ontology over time (using data from each release).
#'
#' @param release_file The path to the file containing DO release details, as a
#'     string.
#' @param counts_file The path to the file containing the count of DO terms
#'     and definitions by release, as a string.
#' @param out_dir The directory where the plot `"{date}-DO_term_def_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, execute:
#'
#' 1. `scripts/DO_term_def_counts.R` - requires installation of a python virtual
#'  environment using `scripts/install_reticulate_python.R`.
#'
#' 2. `scripts/DO_release_details.R`
#'
#' @export
plot_term_def_counts <- function(
    release_file = "data/DO_release/DO_release_details.csv",
    counts_file = "data/DO_release/DO_term_def_counts.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_term_def_count.png"
        )
    )

    release_df <- readr::read_csv(release_file)
    counts_df <- readr::read_csv(counts_file) %>%
        dplyr::rename(release = `...1`)
    df <- dplyr::left_join(
        release_df,
        counts_df,
        by = c("tag_name" = "release")
    ) %>%
        # add year
        dplyr::mutate(date = lubridate::date(created_at)) %>%
        # drop bug fix releases that happen on same day (for plotting by date)
        dplyr::group_by(date) %>%
        dplyr::arrange(desc(created_at)) %>%
        dplyr::filter(!duplicated(date)) %>%
        dplyr::ungroup() %>%
        # drop extra columns
        dplyr::select(date, terms, defs) %>%
        dplyr::mutate(
            n_terms = terms - defs,
            n_defs = defs
        ) %>%
        dplyr::select(-terms, -defs) %>%
        tidyr::pivot_longer(
            cols = c(n_terms, n_defs),
            names_to = "variable",
            values_to = "value"
        ) %>%
        dplyr::mutate(
            variable = factor(
                variable,
                levels = c("n_terms", "n_defs")
            )
        )

    ## Create Area Plot - NEW version, 2021-08-11
    g <- ggplot(df) +
        geom_area(
            aes(x = date, y = value, fill = variable), size = 1
        ) +
        scale_fill_manual(
            name = "Total",
            values = unname(DO_colors[c("light", "default")]),
            labels = c("Terms", "Terms Defined")
        ) +
        scale_y_continuous(
            name = "Count",
            breaks = seq(0, 12000, by = 2000)
        ) +
        scale_x_date(
            name = "Release Date",
            date_breaks = "1 year",
            date_labels = "%Y"
        ) +
        ggtitle("Trend of DO Terms") +
        theme_dark(base_size = 13)

    ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}
