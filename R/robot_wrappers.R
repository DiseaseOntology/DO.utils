#' Convert Ontology Files to OFN
#'
#' Converts ontology files to OWL functional syntax (OFN) using
#' ROBOT (\url{http://robot.obolibrary.org/}).
#'
#' @param path The path to the ontology file, as a string.
#' @param out_path The path of the desired output OFN output file, as a string.
#'     If `NULL` (default), a temporary file will be created.
#' @param gzip Whether output should be gzipped, as a boolean
#'     (default: `FALSE`). If `TRUE`, the output file will end with a '.gz' file
#'     extension.
#' @inheritParams robot
#'
#' @returns The output path of the OFN file, invisibly.
#'
#' @keywords internal
convert_to_ofn <- function(path, out_path = NULL, gzip = FALSE,
                           .robot_path = NULL) {
    stopifnot(
        rlang::is_string(path),
        is.null(out_path) || rlang::is_string(out_path),
        rlang::is_bool(gzip),
        is.null(.robot_path) || rlang::is_string(.robot_path)
    )

    if (is.null(out_path)) {
        out_path <- tempfile(fileext = ".ofn")
    }
    if (gzip && tools::file_ext(out_path) != "gz") {
        out_path <- paste0(out_path, ".gz")
    }

    tools::file_ext(out_path)
    robot("convert", i = path, o = out_path, format = "ofn", .path = .robot_path)
    invisible(out_path)
}


#' Execute a SPARQL Query with ROBOT
#'
#' Wrapper for `robot("query", ...)` that accepts a file _or_ text query, and
#' has more convenient arguments.
#'
#' @param input The path to an RDF/OWL file recognized by ROBOT, as a string.
#' @param query The text for or path to a valid SPARQL query (`ASK`, `SELECT`,
#' `CONSTRUCT`, or `UPDATE`) as a string.
#' @param output The path where output will be written, as a string.
#' @param ... Additional arguments to
#' [ROBOT query](http://robot.obolibrary.org/query) formatted as described in
#' [DO.utils::robot()].
#'
#' @returns `output` invisibly.
#'
#' @seealso [robot()] for underlying implementation; [tidy_sparql()] for tidying
#' tabular output data read into R.
#'
#' @export
robot_query <- function(input, query, output, ...) {
    query_is_file <- file.exists(query)
    if (query_is_file) {
        q <- readr::read_lines(query)
    } else {
        q <- query
        query <- tempfile(fileext = ".sparql")
        readr::write_lines(q, query)
        on.exit(file.remove(query))
    }

    # update query defined by the presence of INSERT/DELETE statements
    update <- any(
            stringr::str_detect(
                q,
                stringr::regex("insert|delete", ignore_case = TRUE)
            )
        )

    if (isTRUE(update)) {
        robot("query", i = input, update = query, o = output, ...)
    } else {
        robot("query", i = input, query = query, output, ...)
    }
    invisible(output)
}
