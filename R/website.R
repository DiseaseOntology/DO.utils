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
#' @param file_in The path to the file containing the list of publications
#'     citing the DO, as a string.
#' @param dir_out The directory where the plot .png should be saved to, as a
#'     string. The file name will be "{date}-DO_cited_by_count.png".
#' @param w The width of the plot in inches, as an integer.
#' @param h The height of the plot in inches, as an integer.
#'
#' @section Data Preparation:
#' To prepare cited by data EXECUTE scripts/citedby_full_procedure.R.
#'
#' @export
plot_citedby <- function(file_in = "data/citedby/DO_citedby.csv",
                         dir_out = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        dir_out,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_cited_by_count.png"
        )
    )

    cited_by <- readr::read_csv(file_in) %>%
        dplyr::mutate(Year = lubridate::year(pub_date)) %>%
        dplyr::count(Year, name = "Publications")

    g <- ggplot(data = cited_by) +
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
