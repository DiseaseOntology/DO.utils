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
