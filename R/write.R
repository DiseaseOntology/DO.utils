#' Tests for Write Access
#'
#' Acts as a minimal wrapper around [file.access(mode = 2)][base::file.access]
#' to test whether R has permission to write to files and/or directories.
#'
#' @inheritParams base::file.access
#'
#' @returns Logical vector of length equal to `names`. **NOTE** that this
#' _differs_ from [base::file.access()] which returns an integer vector.
#'
#' @keywords internal
write_access <- function(names) {
    x <- file.access(names, mode = 2)
    dplyr::case_when(
        x == 0 ~ TRUE,
        TRUE ~ FALSE
    )
}

#' Write a Graph to .graphml File
#'
#' Writes a graph object (tidygraph/iGraph) to a file as
#' [GraphML](https://en.wikipedia.org/wiki/GraphML).
#'
#' @param graph A [tidygraph][tidygraph::tidygraph-package] or
#'     [igraph][igraph::igraph-package] object.
#' @param file The file path to write to, as a string. If '.graphml' extension
#'     is missing, it will be appended.
#'
#' @returns Absolute path of .graphml file written.
#'
#' @export
write_graphml <- function(graph, file) {
    if (tools::file_ext(file) != "graphml") {
        file <- paste0(file, ".graphml")
    }
    igraph::write_graph(graph, file, format = "graphml")
    normalizePath(file)
}


#' Write Data to a Google Sheet
#'
#' Specialized methods for writing data created by `DO.utils` to a specified
#' Google Sheet.
#'
#' @param data A data.frame, possibly with a defined method.
#' @inheritParams googlesheets4::write_sheet
#' @param sheet The name to use for the sheet to write into (i.e the tab name
#' in a worksheet). If a date format recognized by [format.Date()] is
#' included in the string, today's date will be added in the specified format
#' and location.
#'
#' For each method, the default is a method-specific term appended with a date
#' format lacking separators (e.g. 20250101 for 2025-01-01).
#'
#' **_WARNING:_** If a sheet with the same name exists in the Google Sheet file
#' it will be overwritten.
#'
#' @param hyperlink_curie <[`tidy-select`][tidyr::tidyr_tidy_select]> The
#' columns with CURIEs to convert to hyperlinks when written in Google Sheets.
#' @param ... Arguments passed on to methods.
#' @param sheet_nm _DEPRECATED_, use `sheet` instead.
#' @param datestamp _DEPRECATED_, use `sheet` instead.
#'
#' @returns The input `ss`, as an instance of [googlesheets4::sheets_id].
#'
#' @export
write_gs <- function(data, ss, sheet = NULL, hyperlink_curie = NULL, ...) {
    stopifnot(
        "`sheet` must be a character string or `NULL`" =
            is.null(sheet) || rlang::is_string(sheet)
    )
    dot_nm <- names(list(...))
    if (any(dot_nm %in% c("sheet_nm", "datestamp"))) {
        rlang::abort(
            "`sheet_nm` and `datestamp` are deprecated; use `sheet` instead.",
            class = "deprecated"
        )
    }
    UseMethod("write_gs")
}

#' @rdname write_gs
#' @export
write_gs.omim_inventory <- function(data, ss, sheet = "omim_inventory-%Y%m%d",
                                    hyperlink_curie = c("omim", "doid"), ...) {
    gs_info <- write_gs.data.frame(
        data = data,
        ss = ss,
        sheet = sheet,
        hyperlink_curie = hyperlink_curie,
        ...
    )
    invisible(gs_info)
}

#' @rdname write_gs
#' @export
write_gs.data.frame <- function(data, ss, sheet = "data-%Y%m%d",
                                hyperlink_curie = NULL, ...) {
    if (!is.null(sheet)) sheet <- format(Sys.Date(), sheet)

    hyperlink_col <- tidyselect::eval_select(
        tidyselect::enquo(hyperlink_curie),
        data
    )
    if (length(hyperlink_col) > 0) {
        data <- dplyr::mutate(
            data,
            dplyr::across(
                .cols = dplyr::all_of(hyperlink_col),
                .fns = ~ hyperlink_curie(.x, as = "gs")
            )
        )
    }

    gs_info <- googlesheets4::write_sheet(data, ss, sheet)

    invisible(gs_info)
}
