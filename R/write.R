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
#' @param hyperlink_curie <[`tidy-select`][tidyr::tidyr_tidy_select]> The
#' columns with CURIEs to convert to hyperlinks when written in Google Sheets.
#' @param ... Arguments passed on to methods.
#'
#' @returns The data as written to the Google Sheet, invisibly.
#' @export
write_gs <- function(data, ss, hyperlink_curie = NULL, ...) {
    UseMethod("write_gs")
}

#' @rdname write_gs
#'
#' @param datestamp `NULL` or `NA` to use the default sheet name
#' ('omim_inventory') or a format recognized by [format.Date()] to add a date
#' stamp suffix, separated by '-', to the default sheet name.
#'
#' @export
write_gs.omim_inventory <- function(data, ss,
                                    hyperlink_curie = c("omim", "doid"),
                                    datestamp = "%Y%m%d", ...) {
    hyperlink_curie <- tidyselect::eval_select(
        tidyselect::enquo(hyperlink_curie),
        data
    )
    if (length(hyperlink_curie) > 0) {
        data <- dplyr::mutate(
            data,
            dplyr::across(
                .cols = {{ hyperlink_curie }},
                .fns = ~ build_hyperlink(
                    x = stringr::str_remove(.x, ".*:"),
                    url = stringr::str_remove(.x, ":.*"),
                    text = .x,
                    as = "gs"
                )
            )
        )
    }

    sheet_nm <- "omim_inventory"
    if (!is.null(datestamp) && !is.na(datestamp)) {
        sheet_nm <- paste(sheet_nm, format(Sys.Date(), datestamp), sep = "-")
    }

    googlesheets4::write_sheet(
        data = data,
        ss = ss,
        sheet = sheet_nm
    )

    invisible(data)
}
