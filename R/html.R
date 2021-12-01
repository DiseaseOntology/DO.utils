#' Arrange HTML in Rows
#'
#' Arrange html elements in rows, with each row containing the specified number
#' of elements `per_row`.
#'
#' @param cell_html HTML to use in cells, as a character vector.
#' @param per_row The number of cells per row, as an integer.
#' @param tab_indent The number of tabs to use to indent row html code. Only
#'     used to make html more readable. NOTE that cell html will be indented
#'     with one more tab to make rows more apparent.
#' @noRd
html_in_rows <- function(cell_html, per_row = 3, tab_indent = 2) {
    # indent html elements
    r_start <- paste0(rep('\t', tab_indent), '<tr>')
    r_end <- paste0(rep('\t', tab_indent), '</tr>')
    cell_indent <- paste0(rep('\t', tab_indent + 1), cell_html)

    # format rows
    cell_grouped <- partition(cell_indent, n = per_row)
    row_cell_html <- purrr::map(
        cell_grouped,
        ~ c(r_start, .x, r_end)
    ) %>%
        unlist()

    row_cell_html
}
