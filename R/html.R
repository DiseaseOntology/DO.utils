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


#' Build Hyperlinks
#'
#' Builds hyperlinks for Google Sheets, Excel, or HTML by appending a value to
#' the end of a base url.
#'
#' @inheritParams append_to_url
#' @inheritParams format_hyperlink
#' @param text _(OPTIONAL)_ The text to display for each link, as a character
#' vector. The default uses `x` as the text. If `NULL`, the full URL will serve
#' as the text.  If a string, the value will be used for the text of each
#' hyperlink.
#' @param preserve The value to return when `url` is `NA`, as a string. One of
#' "url" or "text" (default). Note that the default for `build_hyperlink()`
#' is opposite the default of [format_hyperlink()] because `text` is provided by
#' default.
#'
#' @seealso Functions used internally: [append_to_url()] and
#'     [format_hyperlink()].
#'
#' @examples
#' build_hyperlink(
#'     x = "DiseaseOntology",
#'     url = "github",
#'     as = "html",
#'     text = "A hyperlink!"
#' )
#'
#' # create CURIE links by passing local identifiers as `x` and prefixes as `url`
#' build_hyperlink(
#'     x = c("4", "D004194"),
#'     url = c("DOID", "MESH"),
#'     as = "gs",
#'     text = c("DOID:4", "MESH:D004194")
#' )
#'
#' # provide internal URL names or direct URLs to append to
#' # BE SURE to use `preserve = 'url'` when text is `NA`.
#' build_hyperlink(
#'     x = c("4", "fakeID"),
#'     url = c("DOID", "https://madeup.url.com"),
#'     as = "gs",
#'     text = c("DOID:4", NA),
#'     sep = c("_", "/"),
#'     preserve = "url"
#' )
#'
#' @export
build_hyperlink <- function(x, url, as, ..., sep = "", text = x,
                            preserve = "text") {
    full_url <- append_to_url(x, url, sep)
    hyperlink <- format_hyperlink(
        full_url,
        as = as,
        ...,
        text = text,
        preserve = preserve
    )

    hyperlink
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
