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


#' Construct HTML `img` Tag(s)
#'
#' Vectorized construction of one or more HTML `img` tags with the specified
#' attributes. `src` & `alt` are required. Where they are missing, no `img`
#' tag will be created. Additional attributes should be the same length as `src`
#' or length <= 1. Length‑1 attributes are recycled.
#'
#' @param src The image source(s), as a character vector.
#' @param alt The alternate text, as a character vector.
#' @inheritParams set_html_attr
#'
#' @returns A character vector of HTML `img` tags.
#'
#' @export
as_html_img <- function(src, alt, ..., quote = "\"") {
    src_len <- length(src)
    alt_len <- length(alt)
    if (src_len < 1 | alt_len < 1 | src_len != alt_len) {
        rlang::abort("`src` and `alt` are required and must be the same length")
    }

    arg_len <- purrr::map_int(list(...), length)
    if (any(arg_len > 1 & arg_len != src_len)) {
        rlang::abort("Additional `...` attributes must be length-1 or the same lenght as `src`")
    }
    attrs <- set_html_attr(src = src, alt = alt, ..., quote = quote)
    attrs[is.na(src) | is.na(alt)] <- NA_character_
    dplyr::if_else(!is.na(attrs), paste0('<img', attrs, '>'), NA_character_)
}


# Internal helpers --------------------------------------------------------

#' Set HTML Attributes
#'
#' Sets attributes for one or more HTML tags in a vectorized manner.
#'
#' @param ... Named character vector(s) or name-value pairs of HTML attributes.
#' Attributes with values of `NULL` or `NA` will be dropped. Names should
#' correspond exactly to the desired HTML attributes.
#'
#' For historical reasons, input may also be a single character vector of
#' name-value pairs.
#'
#' @param quote The quote with which to surround the attribute values. Use `""`
#' if quotes are not desired (default: `"\""`, double quote).
#' @inheritParams check_html_attr
#'
#' @section Notes:
#' * Each input to `...` and `quote` must be a length‑1 vector, a vector of the
#' same length as the longest input, or a vector of length equal to
#' `max_length`. Length‑1 vectors are recycled.
#'
#' * No checking is done to confirm names correspond to true HTML attributes.
#' Beware of spelling errors!
#'
#' @returns A character vector of HTML attribute strings including necessary
#' quotes and with a leading space, e.g.
#' `' src="img path" alt="img alt text here"'`.
#'
#' @keywords internal
set_html_attr <- function(..., max_length = NULL, quote = "\"") {
    attr_list <- check_html_attr(..., max_length = max_length)
    if (is.null(attr_list)) return(invisible(NULL))
    if (is.null(max_length)) max_length <- attr(attr_list, "max_length")

    q_len <- length(quote)
    if (q_len != 1 & q_len != max_length) {
        rlang::abort("`quote` must be length‑1 or the same length as the longest attribute")
    }

    attr_list <- purrr::map2(
        attr_list,
        names(attr_list),
        ~ if (length(.x) == 1) {
            purrr::set_names(rep(.x, max_length), rep(.y, max_length))
        } else {
            purrr::set_names(.x, rep(.y, length(.x)))
        }
    )

    if (q_len == 1) {
        attr_list[["quote"]] <- rep(quote, max_length)
    } else {
        attr_list[["quote"]] <- quote
    }

    out <- purrr::pmap_chr(
        attr_list,
        function(..., q) {
            dplyr::coalesce(paste_html_attr(...), NA_character_)
        }
    )
    unname(out)
}

#' Check HTML Attribute Inputs
#'
#' Checks that all HTML attribute inputs are valid, meaning (1) either
#' length <= 1 or the same length as the longest attribute, (2) that all
#' attributes are named, and (3) that no attributes are duplicated.
#'
#' @inheritParams set_html_attr
#' @param max_length The maximum expected length of attributes, as an integer. If
#' `NULL` (default), the maximum length will be the length of the longest
#' attribute.
#'
#' @keywords internal
check_html_attr <- function(..., max_length = NULL) {
    attr_list <- list(...)
    attr_nm <- names(attr_list)
    # try as if attributes were passed as a single character vector
    if (is.null(attr_nm) & length(attr_list) == 1) {
        attr_list <- as.list(attr_list[[1]])
        attr_nm <- names(attr_list)
    }

    empty_attr <- purrr::map_lgl(attr_list, ~ length(stats::na.omit(.x)) == 0)
    attr_list <- attr_list[!empty_attr]
    if (length(attr_list) == 0) return(invisible(NULL))

    attr_len <- purrr::map_int(attr_list, length)
    if (is.null(max_length)) max_length <- max(attr_len)
    if (any(attr_len > 1 & attr_len != max_length)) {
        rlang::abort(
            "All attributes must have length <= 1 or be of the same length as all length > 1 html inputs"
        )
    }

    if (!rlang::is_named(attr_list)) {
        rlang::abort("All attributes must be named")
    }
    attr_nm_dup <- table(attr_nm) > 1
    if (any(attr_nm_dup)) rlang::abort("Attribute names must be unique")

    attr(attr_list, "max_length") <- max_length
    attr_list
}

paste_html_attr <- function(..., quote = "\"") {
    .attr <- c(...)
    .attr <- .attr[!is.na(.attr)]
    paste0(
        # to add spec required space before all attributes
        " ",
        paste0(names(.attr), '=', sandwich_text(.attr, quote)),
        collapse = ""
    )
}

indent_html <- function(n) {
    collapse_to_string(rep('  ', n), delim = "")
}
