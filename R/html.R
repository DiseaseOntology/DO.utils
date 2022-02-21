#' Arrange HTML in Rows
#'
#' Arrange html elements in rows, with each row containing the specified number
#' of elements `per_row`.
#'
#' @param cell_html HTML to use in cells, as a character vector.
#' @param row_attr HTML attributes to set for rows, as a character vector (see
#'     NOTE).
#' @param cell_attr HTML attributes to set for cells, as a character vector
#'     (see NOTE).
#' @param per_row The number of cells per row, as an integer.
#' @param tab_indent The number of tabs to use to indent row html code. Only
#'     used to make html more readable. NOTE that cell html will be indented
#'     with one more tab to make rows more apparent.
#'
#' @section NOTE:
#' As currently coded, it is not possible to specify attributes separately for
#' each row/cell. All attributes are applied to each row/cell.
#'
#' @examples
#' html_in_rows(c("<b>Hi!</b>", "", "", "What's", "your", "name"))
#'
#' @noRd
html_in_rows <- function(cell_html, row_attr = NULL,
                         cell_attr = NULL, per_row = 3, tab_indent = 2) {

    # format elements as rows & cells
    r_start <- cast_to_string(
        rep('\t', tab_indent),
        cast_to_string('<tr', row_attr, delim = " "),
        '>',
        delim = ""
    )
    r_end <- cast_to_string(rep('\t', tab_indent), '</tr>', delim = "")
    cell_indent <- paste0(
        cast_to_string(rep('\t', tab_indent + 1), delim = ""),
        cast_to_string('<td', cell_attr, delim = " "),
        '>',
        cell_html,
        '</td>',
        delim = ""
    )

    # format rows
    cell_grouped <- partition(cell_indent, n = per_row)
    row_cell_html <- purrr::map(
        cell_grouped,
        ~ c(r_start, .x, r_end)
    ) %>%
        unlist(use.names = FALSE)

    row_cell_html
}


# helpers -----------------------------------------------------------------

indent_html <- function(n) {
    cast_to_string(rep('  ', n), delim = "")
}


set_html_attr <- function(attr) {
    if (is.null(attr)) return(NULL)

    cast_to_string(
        # to add space between html element and attributes
        "",
        paste0(names(attr), '="', attr, '"'),
        delim = " "
    )
}
