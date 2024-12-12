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
#' @param data A specially-classed data.frame with a defined method.
#' @inheritParams googlesheets4::write_sheet
#' @param sheet The name to use for the sheet to write into, in the sense of
#' "worksheet" or "tab". If a date format recognized by [format.Date()] is
#' included in the string, today's date will be added in the specified format
#' and location. If `NULL`, the sheet name will be the default for the
#' underlying method.
#'
#' **_WARNING:_** If a sheet with the same name exists it will be overwritten.
#' @param hyperlink_curie <[`tidy-select`][tidyr::tidyr_tidy_select]> The
#' columns with CURIEs to convert to hyperlinks when written in Google Sheets.
#' @param ... Arguments passed on to methods.
#'
#' @returns The input `ss`, as an instance of [googlesheets4::sheets_id].
#'
#' @export
write_gs <- function(data, ss, sheet = NULL, hyperlink_curie = NULL, ...) {
    stopifnot(
        "`sheet` must be a character string or `NULL`." =
            is.null(sheet) || rlang::is_string(sheet)
    )
    UseMethod("write_gs")
}

#' @rdname write_gs
#' @param datestamp **DEPRECATED** Use `sheet` instead.
#'
#' Previously `NULL` or `NA` would default to the sheet name 'omim_inventory',
#' while a format recognized by [format.Date()] would add an additional date
#' stamp suffix, separated by '-', to the default sheet name. This behavior is
#' retained for backward compatibility and invoked when `sheet = NULL`.
#'
#' @export
write_gs.omim_inventory <- function(data, ss, sheet = "omim_inventory-%Y%m%d",
                                    hyperlink_curie = c("omim", "doid"),
                                    datestamp = "%Y%m%d", ...) {
    hyperlink_col <- tidyselect::eval_select(
        tidyselect::enquo(hyperlink_curie),
        data
    )
    if (length(hyperlink_col) > 0) {
        data <- dplyr::mutate(
            data,
            dplyr::across(
                .cols = {{ hyperlink_col }},
                .fns = ~ hyperlink_curie(.x, as = "gs")
            )
        )
    }

    if (is.null(sheet)) {
        sheet_nm <- "omim_inventory"
        if (!is.null(datestamp) && !is.na(datestamp)) {
            rlang::warn(
                "The `datestamp` argument is deprecated. Use `sheet` instead or set `datestamp` to `NULL`."
            )
            sheet_nm <- paste(sheet_nm, format(Sys.Date(), datestamp), sep = "-")
        }
    } else {
        sheet_nm <- format(Sys.Date(), sheet)
    }

    gs_info <- googlesheets4::write_sheet(data, ss, sheet_nm)

    invisible(gs_info)
}
