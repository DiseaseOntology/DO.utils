#' Sandwich Text Between Placeholders
#'
#' Sandwiches strings between one or two placeholders.
#'
#' @param x A string or character vector.
#' @param placeholder One or two placeholders to sandwich each element of `x`
#'     between. When two placeholders are provided, `x` will be sandwiched
#'     between them with the first at the start and second at the end.
#'     Otherwise, `x` will be sandwiched at both start and end by the same
#'     placeholder.
#' @param add_dup Whether to add placeholders even if the same character is
#'     already found in that position, as a boolean (default: `TRUE`).
#'
#' @examples
#' sandwich_text("a", placeholder = "h")
#' sandwich_text("a", placeholder = c("b", "h"))
#' sandwich_text("bah", placeholder = c("b", "h"), add_dup = TRUE)
#' sandwich_text("bah", placeholder = c("b", "h"), add_dup = FALSE)
#' sandwich_text("bah", placeholder = "h", add_dup = FALSE)
#'
#' @family general utilities
#' @export
sandwich_text <- function(x, placeholder, add_dup = TRUE) {
    if (length(placeholder) < 1 ||
        length(placeholder) > 2 ||
        !is.character(placeholder)
    ) {
        stop("`placeholder` must be a length-1 or -2 character vector.")
    }

    # str_replace_all code from stringr::str_escape() without $, because
    #   stringr::str_replace() already has special handling to escape $
    placeholder2 <- stringr::str_replace_all(
        rep(placeholder, length(placeholder) %% 2 + 1),
        "([.^\\\\|*+?{}\\[\\]()])",
        "\\\\\\1"
    )
    if (add_dup) {
        pattern <- c("^", "$")
    } else {
        opt_placeholder <- paste0(placeholder2, "?")
        pattern <- paste0(c("^", opt_placeholder[2]), c(opt_placeholder[1], "$"))
    }

    out <- x
    purrr::map2(
        pattern,
        placeholder2,
        function(.p, .r) out <<- stringr::str_replace(out, .p, .r)
    )

    if (!is.null(names(x))) {
        names(out) <- names(x)
    }

    out
}


#' Sort by Character Length
#'
#' Sort a vector (`length_sort()`) or data.frame (`length_order()`) by
#' character length. Multiple elements of the the same length are secondarily
#' sorted by order of appearance.
#'
#' @param x A vector.
#' @param by_name Whether to sort a vector by name instead of value, as a
#'     boolean.
#' @param data A data.frame.
#' @param cols <[`tidy-select`][tidyr::tidyr_tidy_select]> The columns of `data`
#' to order by.
#' @inheritDotParams base::order decreasing na.last method
#' @examples
#' # Sorting vectors
#' x <- c("ccc", "aaaa", "eee", "b", "DDD")
#' length_sort(x)
#' length_sort(x, decreasing = TRUE)
#'
#' x2 <- c(1:9, NA, 100, 10)
#' length_sort(x2)
#' length_sort(x2, decreasing = TRUE)
#' length_sort(x2, na.last = NA)
#'
#' x3 <- c(bb = 333, ccc = 1, a = 22)
#' length_sort(x3, by_name = TRUE)
#'
#' # Ordering data.frames
#' x <- tibble::tibble(
#'     x = 1:3,
#'     y = c("b", "aa", "c"),
#'     z = c("bb", "a", "c")
#' )
#'
#' length_order(x, "y")
#' length_order(x, "z")
#' length_order(x, c("y", "z"))
#' length_order(x, c("y", "z"), decreasing = TRUE)
#'
#' @family general utilities
#' @export
length_sort <- function(x, by_name = FALSE, ...) {
    if (by_name) {
        len <- stringr::str_length(names(x))
    } else {
        len <- stringr::str_length(x)
    }

    x[order(len, ...)]
}

#' @rdname length_sort
#' @export
length_order <- function(data, cols, ...) {
    .cols <- tidyselect::eval_select(rlang::enquo(cols), data)
    if (length(.cols) > 1) {
        index <- do.call(
            "order",
            c(
                purrr::map(
                    dplyr::select(data, dplyr::all_of(.cols)),
                    stringr::str_length
                ),
                ...
            )
        )
    } else {
        index <- order(stringr::str_length(dplyr::pull(data, .cols)), ...)
    }
    data[index, ]
}



#' Suggest a Regular Expression That Will Match All Input
#'
#' Collects the full set of characters found at each position across all strings
#' in `x` and returns it as a quasi-regular expression. Letter and numbers will
#' not be condensed to ranges in output, even if the full sets are present at a
#' position.
#'
#' @param x A character vector.
#' @param pivot Whether the details `tibble` should be in "wide" (default) or
#' "long" format. See Details.
#'
#' @returns A string of class `suggested_regex`, including detailed information
#' in a `details` attribute, which is displayed when printed.
#'
#' @section Details:
#' When `pivot = "long"`, details will be formatted as a tidy `tibble` with 3
#' columns and as many rows as the string length of the longest input:
#' 1. `position`: indicating the position of the character set in the input.
#' 2. `regex`: giving the character set (in brackets),
#' 3. `n`: the count of input strings that have a character at that `position`.
#'
#' When `pivot = "wide"` (default), a `tibble` with the same information
#' organized into rows (1 header and 2 normal rows) corresponding to the 3
#' columns described.
#'
#' @examples
#' x <- c("DNA", "MHC", "TAP1", "TAP2", "520", "ACD")
#'
#' suggest_regex(x)
#' suggest_regex(x, "long")
#'
#' @family general utilities
#' @seealso [print.suggested_regex()] for the print method.
#' @export
suggest_regex <- function(x, pivot = "wide") {
    pivot <- match.arg(pivot, choices = c("wide", "long"))
    details <- tibble::tibble(position = stringr::str_length(x)) |>
        dplyr::count(.data$position)
    max_len <- max(details$position)
    missing_pos <- (1:max_len)[!1:max_len %in% details$position]
    details <- details |>
        tibble::add_row(position = missing_pos, n = max(details$n)) |>
        dplyr::arrange(.data$position)

    xsplit <- stringr::str_split(x, "")

    chr_at_pos <- purrr::map(
        xsplit,
        ~ append(.x, rep(NA, max_len - length(.x)))
    ) |>
        invert_sublists() |>
        purrr::map_chr(
            function(.x) {
                candidates <- sort(unique(unlist(.x)))
                if (length(candidates) == 1) return(candidates)
                sandwich_text(
                    collapse_to_string(
                        candidates,
                        delim = "",
                        na.rm = TRUE,
                        unique = TRUE
                    ),
                    c("[", "]")
                )
            }
        )

    details <- details |>
        dplyr::mutate(regex = chr_at_pos, .after = "position")

    out <- paste0(details$regex, collapse = "")

    if (pivot == "wide") {
        details <- details |>
            dplyr::mutate(n = as.character(.data$n)) |>
            dplyr::rename(pos = "position") |>
            tidyr::pivot_longer(
                cols = c("regex", "n"),
                names_to = "position",
                values_to = "value"
            ) |>
            tidyr::pivot_wider(
                names_from = "pos",
                values_from = "value"
            )
    }
    class(out) <- c("suggested_regex", class(out))
    attr(out, "details") <- details
    out
}


#' Calculate Maximum Parentheses Depth
#'
#' Calculates the maximum depth of nested parentheses in a string or character
#' vector.
#'
#' @param x A character vector.
#' @param unmatched_err Whether to signal an error or return `NA` for strings
#' with unmatched parentheses,
#'
#' @returns An integer vector of the same length as the input giving the maximum
#' depth of nested parentheses in each element. `NA` will bStrings with unmatched
#' parentheses will return `NA`
#'
#' @examples
#' max_paren_depth(c("no parens", "a (1 deep)", "((a) and (b or (3 deep)))"))
#'
#' # errs by default
#' tryCatch(
#'   max_paren_depths("unmatched ( paren"),
#'   error = function(e) print(e$message)
#' )
#'
#' # allow `NA` output for unmatched parentheses
#' max_paren_depth("unmatched ( paren", unmatched_err = FALSE)
#' max_paren_depth(
#'   c("a (1 deep)", "((a) and (b or (3 deep)))", "unmatched ( paren"),
#'   unmatched_err = FALSE
#' )
#'
#' @family general utilities
#' @export
max_paren_depth <- function(x, unmatched_err = TRUE) {
    str_paren_depth <- function(s, unmatched_err = TRUE) {
        stopifnot(
            "`unmatched_err` must be a boolean" = rlang::is_bool(unmatched_err)
        )
        chars <- strsplit(s, "")[[1]]
        paren <- ifelse(chars == "(", 1L, ifelse(chars == ")", -1L, 0L))
        if (length(paren) == 0) return(0L)
        cum <- cumsum(paren)
        if (any(cum < 0) || utils::tail(cum, 1) != 0) {
            if (unmatched_err) {
                stop("Unmatched parentheses detected in string: '", s, "'")
            }
            return(NA_integer_)
        }
        max(c(0L, cum))
    }
    vapply(x, str_paren_depth, integer(1), unmatched_err = unmatched_err)
}


############################ INTERNAL UTILITIES ###############################

glueV <- function(..., .envir = parent.frame()) {
    glue::glue(..., .envir = .envir, .open = "!<<", .close = ">>!")
}


#' Calculate a Rolling/Windowed Middle (Mean) Value
#'
#' Calculates a rolling/windowed mean between two values using an additional
#' `limit` value for the first or last calculation.
#'
#' @param x A numeric vector.
#' @param limit A value that will be added to the start or end of `x`, as a
#' numeric scalar.
#' @param limit_type Specify whether `limit` should be used as the `"min"`
#' (default) or `"max"` value.
#'
#' @returns A numeric vector of middle values of the same length as `x`.
#' @examples
#' \dontrun{
#' x <- c(7, 14, 21, 25)
#'
#' roll_middle(x, 0, "min")
#' roll_middle(x, 30, "max")
#' }
#'
#' @section Notes:
#' An alternative function that produces the same result when the `limit` is
#' added at the start or end of the input `x` (e.g. `c(limit, x)` or
#' `c(x, limit)`) is `roll_mean(x, n = 2)` from the `RccpRoll` package.
#'
#' @keywords internal
roll_middle <- function(x, limit, limit_type = "min") {
    limit_type <- match.arg(limit_type, choices = c("min", "max"))
    stopifnot(
        "`x` must be a numeric vector sorted in ascending order" = x == sort(x),
        "`limit` must be a numeric scalar" =
            length(limit) == 1 && is.numeric(limit)
    )

    if (limit_type == "min") {
        stopifnot(
            "`limit` must be <= min(x) when `limit_type = 'start'`" = limit <= min(x)
        )
        x_rng <- c(limit, x[-length(x)])
        out <- (x - x_rng) / 2 + x_rng
    } else {
        stopifnot(
            "`limit` must be >= max(x) when `limit_type = 'end'`" = limit >= max(x)
        )
        x_rng <- c(x[-1], limit)
        out <- (x_rng - x) / 2 + x
    }

    out
}


#' Replace Arabic Numbers with Roman Equivalent
#'
#' Converts arabic numbers _embedded in strings_ to roman numerals to support
#' string comparison where arabic and roman numerals may be used
#' interchangeably. The reverse, identifying roman numerals and converting to
#' arabic numbers is much harder and less precise. _Replacement is not suitable_
#' _when arabic numbers contain ANY separators (e.g. commas, decimals, etc.)._
#'
#' @param x A character vector.
#'
#' @noRd
arabic_to_roman <- function(x) {
    numbers <- stringr::str_extract_all(x, "[0-9]+") |>
        unlist() |>
        unique() |>
        length_sort(decreasing = TRUE)

    if (length(numbers) == 0) return(x)

    replace_vctr <- as.character(utils::as.roman(numbers))
    names(replace_vctr) <- numbers
    stringr::str_replace_all(x, replace_vctr)
}


#' Match Length-2+ Vector Arguments
#'
#' Matches arguments with several inputs allowed against a set of choices.
#' Similar to [base::match.arg()] with `several.ok = TRUE` _EXCEPT_
#' `match_arg_several()` will signal an error for unmatching values in `arg` instead
#' of silently dropping them.
#'
#' @param arg Function argument, as a character vector, or `NULL`.
#' @param choices Candidate values, as a character vector.
#'
#' @keywords internal
match_arg_several <- function(arg, choices) {
    arg_nm <- rlang::as_string(rlang::enexpr(arg))
    arg_missing <- !arg %in% choices

    if (any(arg_missing)) {
        if (is.character(choices)) {
            choices <- sandwich_text(choices, '"')
        }

        msg <- paste0(
            "`", arg_nm, "` must be one of: ",
            vctr_to_string(choices, ", ")
        )
        arg_err <- arg[arg_missing]
        if (is.character(arg)) {
            arg_err <- sandwich_text(arg[arg_missing], '"')
        }
        x_err <- wrap_onscreen(
            paste0(
                "Not ",
                paste0(
                    arg_err, " (pos: ", which(arg_missing), ")",
                    collapse = ", "
                )
            ),
            exdent = 0
        )
        rlang::abort(
            c(msg, x = x_err),
            call = parent.frame()
        )
    }

    arg
}


# For producing messages --------------------------------------------------

# concatenates vector input `x` replacing values with the quantity dropped
# when `x` is longer than `n` (optionally dropping duplicates)
trunc_cat_n <- function(x, n, delim = ", ", unique = TRUE, na.rm = FALSE,
                        sort = FALSE, decreasing = FALSE, ...) {
    if (unique) x <- unique(x)
    if (length(x) > n) {
        x_cat <- vctr_to_string(
            x[1:n],
            delim = delim,
            na.rm = na.rm,
            sort = sort,
            decreasing = decreasing,
            ...
        )
        msg <- paste0(x_cat, " [+", length(x) - n, " more]")
    } else {
        msg <- vctr_to_string(
            x,
            delim = delim,
            na.rm = na.rm,
            sort = sort,
            decreasing = decreasing,
            ...
        )
    }

    msg
}


# creates a message for `...` that shows the values in it as part of the message,
# when applicable (using .which arg)
#
# .which = logical/numeric vector indicating positions in `...` to include
#   in message
msg_dots <- function(.msg, ..., .which = NULL, .bullet = NULL) {
    dots <- rlang::exprs(...)
    arg <- msg_dots_(dots)
    if (!is.null(.bullet)) {
        arg <- purrr::set_names(arg, nm = rep(.bullet, length(arg)))
    }
    if (!is.null(.which)) {
        if (is.logical(.which) && length(.which) != length(dots)) {
            rlang::abort("Logical .which must be same length as `...`")
        }
        if (is.numeric(.which)) {
            in_rng <- dplyr::between(.which, 1, length(dots))
            if (!all(in_rng)) {
                rlang::abort(
                    c(
                        "Numeric .which must all be <= length of `...`",
                        i = paste0("Not: ", to_range(.which[!in_rng]))
                    )
                )
            }
        }
        arg <- arg[.which]
    }

    c(.msg, arg)
}


msg_dots_ <- function(x) {
    x_val <- dplyr::if_else(
        purrr::map_lgl(x, rlang::is_string),
        paste0('"', x, '"'),
        as.character(x)
    )
    dplyr::if_else(
        names(x) == "",
        x_val,
        paste(names(x), x_val, sep = " = ")
    )
}


# For creating code-formatted text in manuals programmatically ------------

# makes elements in `x` appear as code in documentation
#   see R/data.R DO_colors for examples on how this works
vctr_to_mancode <- function(x, regex = NULL, use_names = FALSE) {
    if (use_names) x <- names(x)
    if (!is.null(regex)) x <- x[stringr::str_detect(x, regex)]
    # vctr_to_string(
        sandwich_text(x, c('\\code{', '}'))#, ", ")
}


# converts a named list to a list for R documentation, ordered controls whether
# upper-level list items are bulleted or numbered, converts from
# list(a = 1:2, b = letters[1:2]) to
# * a: 1, 2
# * b: "a", "b"
# See get_url() for example
list_to_man <- function(x, ordered = FALSE) {
    if (any(!rlang::have_name(x))) {
        rlang::abort("All elements in `x` must be named.")
    }
    list_item <- if (ordered) {
        paste0(1:length(names(x)), ". ")
    } else {
        "* "
    }

    man_list <- paste0(
        list_item,
        names(x), ": ",
        purrr::map_chr(x, ~ vctr_to_string(.x, delim = ", "))
    )

    vctr_to_string(man_list, delim = "\n")
}


# For updating internal data store ----------------------------------------

#' Update sysdata.rda retaining existing data
#'
#' Adds objects to the internal data store, `sysdata.rda`, optionally
#' overwriting existing objects. Works whether `sysdata.rda` exists or not and
#' will ONLY overwrite objects that are specified in the `...` argument.
#'
#' @param ... Objects to add to the internal data store.
#' @param overwrite Whether to overwrite the data objects in `...` if they
#' already exist in the internal data store.
#' @inheritParams save
#'
#' @noRd
use_data_internal <- function(..., overwrite = FALSE, compress = "bzip2",
                              version = 2, ascii = FALSE) {
    dots_as_strings <- rlang::enexprs(...) %>%
        purrr::map_chr(rlang::as_string)

    dots_not_exist <- purrr::map_lgl(dots_as_strings, ~ !exists(.x))
    if (any(dots_not_exist)) {
        rlang::abort(
            c(
                "Specified data could not be found:",
                purrr::set_names(
                    dots_as_strings[dots_not_exist],
                    rep("x", sum(dots_not_exist))
                )
            )
        )
    }
    if (file.exists("R/sysdata.rda")) {
        sysdata <- load("R/sysdata.rda")

        obj_exist <- intersect(dots_as_strings, sysdata)
        if (length(obj_exist) > 0 && !overwrite) {
            rlang::abort(
                c(
                    "Internal data already exists. Use `overwrite = TRUE` to overwrite.",
                    purrr::set_names(obj_exist, rep("x", length(obj_exist)))
                )
            )
        }
    } else {
        sysdata <- character(0)
    }

    save(
        list = union(sysdata, dots_as_strings),
        file = "R/sysdata.rda",
        compress = compress,
        version = version,
        ascii = ascii,
        envir = parent.frame()
    )
}
