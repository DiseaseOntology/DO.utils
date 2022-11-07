#' Arrange HTML in Rows
#'
#' Arrange html elements in rows, with each row containing the specified number
#' of elements `per_row`.
#'
#' @param cell_html HTML to use in cells, as a character vector.
#' @param row_attr Name-value pairs of HTML attributes to set for rows, as a
#'     named character vector (see NOTE).
#' @param cell_attr Name-value pairs of HTML attributes to set for cells, as a
#'     named character vector (see NOTE).
#' @param per_row The number of cells per row, as an integer.
#' @param indent_n The number of 2-space indents to use for row html code
#'     (`<tr>` tag). _Only used to make html more readable._ NOTE that cell
#'     html (`<td>` tags) will have one more 2-space indent than `<tr>` tag.
#'
#' @section NOTES:
#' As currently coded, it is not possible to specify attributes separately for
#' each row/cell. All attributes are applied to each row/cell.
#'
#' Formatting here is currently designed to follow the
#' [W3C style guide](https://www.w3schools.com/html/html5_syntax.asp). The
#' [google style guide](https://google.github.io/styleguide/htmlcssguide.html)
#' may be used in the future.
#'
#' @examples
#' html_in_rows(c("<b>Hi!</b>", "", "", "What's", "your", "name"))
#'
#' @noRd
html_in_rows <- function(cell_html, row_attr = NULL,
                         cell_attr = NULL, per_row = 3, indent_n = 2) {

    # format row elements (include attributes)
    r_start <- collapse_to_string(
        indent_html(indent_n),
        '<tr',
        set_html_attr(row_attr),
        '>',
        delim = ""
    )
    r_end <- collapse_to_string(indent_html(indent_n), '</tr>', delim = "")

    # format cell elements (include attributes & html)
    cell <- paste0(
        collapse_to_string(
            indent_html(indent_n + 1),
            '<td',
            set_html_attr(cell_attr),
            '>',
            delim = ""
        ),
        cell_html,
        '</td>'
    )

    # arrange cells in rows
    cell_grouped <- partition(cell, n = per_row)
    row_cell_html <- purrr::map(
        cell_grouped,
        ~ c(r_start, .x, r_end)
    ) %>%
        unlist(use.names = FALSE)

    row_cell_html
}


#' Build an HTML Hyperlink (INTERNAL)
#'
#' Builds a hyperlink formatted as html.
#'
#' @inheritParams append_to_url
#' @param text The text to display for the link, as a character vector.
#' @param target An html `<a>`
#' [target](https://www.w3schools.com/tags/att_a_target.asp) attribute.
#'
#' @examplesIf interactive()
#' build_hyperlink_html(x = "allenbaron", url = "github", text = "A hyperlink!")
#'
#' @keywords internal
build_hyperlink_html <- function(x, url, text, target = "_blank",
                                 preserve_NA = TRUE) {
    full_url <- append_to_url(x, url)

    if (!is.null(target)) {
        html_target <- glue::glue(' target="{target}"')
    } else {
        html_target <- NULL
    }
    href <- glue::glue('<a href="{full_url}"{html_target}>{text}</a>')

    if (preserve_NA) {
        href[is.na(x)] <- NA
    }

    href
}



# html_in_rows() helpers --------------------------------------------------

indent_html <- function(n) {
    collapse_to_string(rep('  ', n), delim = "")
}


set_html_attr <- function(attr) {
    if (is.null(attr)) return(NULL)

    collapse_to_string(
        # to add space between html element and attributes
        "",
        paste0(names(attr), '="', attr, '"'),
        delim = " "
    )
}
