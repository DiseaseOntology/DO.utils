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


# html_in_rows() helpers --------------------------------------------------

indent_html <- function(n) {
    collapse_to_string(rep('  ', n), delim = "")
}

#' Set HTML Attributes
#'
#' These functions set attributes in HTML tags, `set_html_attr()` for a single
#' tag and `map_set_html_attr()` for one or more tags in a vectorized manner.
#'
#' @param ... Named character vector(s) or name-value pairs of HTML attributes.
#' Attributes with values of `NULL` or `NA` will be dropped. Names should
#' correspond exactly to the desired HTML attributes.
#' @param quote The quote with which to surround the attribute values. Use `""`
#' if quotes are not desired (default: `"\""`, double quote).
#'
#' @section Notes:
#' * For `map_set_html_attr()`, each input to `...` and `quote` must be a
#' length-1 vector or a vector of the same length as the longest input. Length-1
#' vectors will be recycled.
#'
#' * No checking is done to confirm names correspond to true HTML attributes.
#' Beware of spelling errors!
#'
#' @returns A character vector of HTML attribute strings including necessary
#' quotes and with a leading space, e.g.
#' `' src="img path" alt="img alt text here"'`.
#'
#' @keywords internal
set_html_attr <- function(..., quote = "\"") {
    attr <- c(...)
    attr <- attr[!is.na(attr)]
    if (length(attr) == 0) return(NULL)
    stopifnot("All `...` must be named" = rlang::is_named(attr))

    collapse_to_string(
        # to add space between html element and attributes
        "",
        paste0(names(attr), '=', sandwich_text(attr, quote)),
        delim = " "
    )
}

#' @rdname set_html_attr
map_set_html_attr <- function(..., quote = "\"") {
    attr_list <- list(...)
    attr_list <- attr_list[purrr::map_lgl(attr_list, ~ !is.null(.x))]
    a_len <- purrr::map_int(attr_list, length)
    max_len <- max(a_len)
    if (any(a_len > 1 & a_len != max_len)) {
        rlang::abort("All attributes must have the same length or length <= 1")
    }
    if (!rlang::is_named(attr_list)) {
        rlang::abort("All attributes must be named")
    }
    attr_nm_dup <- names(attr_list) |>
        table() |>
        (\(x) x > 1)()
    if (any(attr_nm_dup)) rlang::abort("Attribute names must be unique")
    q_len <- length(quote)
    if (q_len != 1 & q_len != max_len) {
        rlang::abort("`quote` must be length-1 or the same length as the longest attribute")
    }
    attr_list <- purrr::map2(
        attr_list,
        names(attr_list),
        ~ if (length(.x) == 1) {
            purrr::set_names(rep(.x, max_len), rep(.y, max_len))
        } else {
            purrr::set_names(.x, rep(.y, length(.x)))
        }
    )
    if (q_len == 1) {
        attr_list[["quote"]] <- rep(quote, max_len)
    } else {
        attr_list[["quote"]] <- quote
    }
    purrr::pmap_chr(
        attr_list,
        function(..., q) {
            dplyr::coalesce(set_html_attr(...), NA_character_)
        }
    )
}
