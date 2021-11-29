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
        dplyr::filter(!is.na(.data$added)) %>%
        # ensure list is alphabetical
        dplyr::arrange(.data$name)

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
        dplyr::mutate(Year = lubridate::year(.data$pub_date)) %>%
        dplyr::count(.data$Year, name = "Publications")

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data$Year, y = .data$Publications),
            width = 0.6,
            fill = DO_colors["light"]
        ) +
        ggplot2::labs(title = "Publications Citing DO", x = "Year", y = "Count") +
        ggplot2::theme_dark(base_size = 13)

    ggplot2::ggsave(
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
        dplyr::rename(release = .data$...1)
    df <- dplyr::left_join(
        release_df,
        counts_df,
        by = c("tag_name" = "release")
    ) %>%
        # add year
        dplyr::mutate(date = lubridate::date(.data$created_at)) %>%
        # drop bug fix releases that happen on same day (for plotting by date)
        dplyr::group_by(.data$date) %>%
        dplyr::arrange(dplyr::desc(.data$created_at)) %>%
        dplyr::filter(!duplicated(.data$date)) %>%
        dplyr::ungroup() %>%
        # drop extra columns
        dplyr::select(.data$date, .data$terms, .data$defs) %>%
        dplyr::mutate(
            n_terms = .data$terms - .data$defs,
            n_defs = .data$defs
        ) %>%
        dplyr::select(-.data$terms, -.data$defs) %>%
        tidyr::pivot_longer(
            cols = c(.data$n_terms, .data$n_defs),
            names_to = "variable",
            values_to = "value"
        ) %>%
        dplyr::mutate(
            variable = factor(
                .data$variable,
                levels = c("n_terms", "n_defs")
            )
        )

    ## Create Area Plot - NEW version, 2021-08-11
    g <- ggplot2::ggplot(df) +
        ggplot2::geom_area(
            ggplot2::aes(x = .data$date, y = .data$value, fill = .data$variable),
            size = 1
        ) +
        ggplot2::scale_fill_manual(
            name = "Total",
            values = unname(DO_colors[c("light", "default")]),
            labels = c("Terms", "Terms Defined")
        ) +
        ggplot2::scale_y_continuous(
            name = "Count",
            breaks = seq(0, 12000, by = 2000)
        ) +
        ggplot2::scale_x_date(
            name = "Release Date",
            date_breaks = "1 year",
            date_labels = "%Y"
        ) +
        ggplot2::ggtitle("Trend of DO Terms") +
        ggplot2::theme_dark(base_size = 13)

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot Branch Counts
#'
#' Plots the count of _non-obsolete_ terms in each major branch of the Human
#' Disease Ontology.
#'
#' @param data_file The path to the file containing the latest DO branch counts,
#'     as a string.
#' @param out_dir The directory where the plot `"{date}-DO_branch_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, manually copy and paste stats from the Google Sheet
#' [DO_github_release_log](https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
#' or from `build/reports/branch-count.tsv` in the local repo where the release
#' was generated.
#'
#' @export
plot_branch_counts <- function(
    data_file = "data/DO_release/branch_counts.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_branch_count.png"
        )
    )

    branch_order <- c(syndrome = "Syndrome",
                      physicalDisorder = "Physical Disorder",
                      genetic = "Genetic Disease",
                      metabolism = "Metabolism",
                      mentalHealth = "Mental Health",
                      cellularProliferation = "Cellular Proliferation",
                      anatomicalEntity = "Anatomical Location",
                      infectiousAgent = "Infectious Disease")

    df <- readr::read_csv(data_file) %>%
        dplyr::mutate(
            DO_branches = factor(
                dplyr::recode(.data$DO_branches, !!!branch_order),
                levels = branch_order
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data$DO_branches, y = .data$Count),
            width = 0.6, fill = DO_colors["light"]
        ) +
        ggplot2::scale_y_continuous(
            breaks = seq(0, round_up(max(df$Count), -3), by = 1000)
        ) +
        ggplot2::coord_flip() +
        ggplot2::ggtitle("DO Branch Counts") +
        ggplot2::theme_dark(base_size = 13) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot Xref Counts
#'
#' Plots the count of cross-references by source in the Human Disease Ontology.
#'
#' @param data_file The path to the file containing the latest DO xref counts,
#'     as a string.
#' @param out_dir The directory where the plot `"{date}-DO_xref_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, manually copy and paste stats from the Google Sheet
#' [DO_github_release_log](https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
#' OR manually copy & paste stats from xref.tsv produced by executing:
#'
#' ```
#' robot query --input {DO_repo}/src/ontology/doid.owl \
#'             --query {DO_repo}/src/sparql/build/all-xref-report.rq xref.tsv
#'```
#'
#' @export
plot_xref_counts <- function(
    data_file = "data/DO_release/cross_references.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_xref_count.png"
        )
    )

    df <- readr::read_csv(data_file) %>%
        dplyr::filter(!is.na(.data$Curation)) %>%
        dplyr::mutate(
            Curation = factor(
                .data$Curation,
                levels = c("Manual", "Mixed", "Automated")
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data$Cross_References, y = .data$Count,
                         fill = .data$Curation),
            width = 0.6
        ) +
        ggplot2::scale_y_continuous(
            breaks = seq(0, round_up(max(df$Count), -3), by = 2000)
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
            values = unname(DO_colors[c("light", "mid", "default")])
        ) +
        ggplot2::labs(title = "DO Cross-References", x ="Cross References") +
        ggplot2::theme_dark(base_size = 13)

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}