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
#' @param output The path where output will be written, as a string, or `NULL`
#' (default) to load data directly. `output` is required for `UPDATE` and
#' `CONSTRUCT` queries.
#' @param ... Additional arguments to
#' [ROBOT query](http://robot.obolibrary.org/query) formatted as described in
#' [DO.utils::robot()].
#' @inheritParams tidy_sparql
#' @inheritParams readr::read_tsv
#'
#' @returns
#' If `output` is specified, the path to the output file with the query result.
#' Otherwise, the query result (ASK as boolean or SELECT as `tibble`).
#'
#' @seealso [robot()] for underlying implementation.
#'
#' @export
robot_query <- function(input, query, output = NULL, ...,
                        tidy_what = "nothing", col_types = NULL) {
    # load query, also ensure query in a file (required by ROBOT)
    query_is_file <- file.exists(query)
    if (query_is_file) {
        q <- readr::read_lines(query)
    } else {
        q <- query
        query <- tempfile(fileext = ".sparql")
        readr::write_lines(q, query)
        on.exit(unlink(query))
    }

    # determine query type (SELECT = default, but not in search since it can be
    # used in other queries)
    q_type <- stringr::str_extract(
        q,
        stringr::regex(
            "\\b(insert|delete|construct|ask)\\b",
            ignore_case = TRUE
        )
    )[1]
    q_type <- stringr::str_to_lower(q_type)
    q_type[is.na(q_type)] <- "select"
    q_type <- switch(
        q_type,
        insert = "update",
        delete = "update",
        q_type
    )

    if (q_type %in% c("update", "construct") && is.null(output)) {
        rlang::abort("`output` is required for CONSTRUCT or UPDATE (INSERT/DELETE) `query`")
    }

    # output handling
    if (is.null(output)) {
        to_stdout <- TRUE
        output <- tmp_out <- tempfile(fileext = ".tsv")
        on.exit(unlink(tmp_out), add = TRUE)
    } else {
        to_stdout <- FALSE
    }

    # handle varying ROBOT parameters for UPDATE queries
    if (q_type == "update") {
        robot("query", i = input, update = query, o = output, ...)
    } else {
        robot("query", i = input, query = query, output, ...)
    }

    # handle output
    if (isFALSE(to_stdout)) {
        return(output)
    }

    if (q_type == "ask") {
        ask_res <- readr::read_file(output)
        out <- switch(ask_res, true = TRUE, false = FALSE, ask_res)
    } else {
        out <- readr::read_tsv(
            output,
            col_types = col_types,
            show_col_types = FALSE
        )
        out <- tidy_sparql(out, tidy_what)
    }
    out
}

#' More powerful diff that relies on ROBOT export instead of robot diff
# robot_diff <- function(old, new, output = NULL) {
#     input <- c(old = old, new = new)
#     temp_tsv <- c(
#         temp1 = tempfile(fileext = ".tsv"),
#         temp2 = tempfile(fileext = ".tsv")
#     )
#     on.exit(unlink(temp_tsv))
#
#     prop_query <- system.file(
#         "sparql/all-class-predicates.rq",
#         package = "DO.utils",
#         mustWork = TRUE
#     )
#     prop <- purrr::map2(
#         input,
#         temp_tsv,
#         ~ vctr_to_string(
#             robot_query(
#                 input = .x,
#                 query = prop_query,
#                 tidy_what = "everything"
#             )$predicate
#         )
#     )
#
#     export <- purrr::pmap(
#         input,
#         prop,
#         temp_tsv,
#         function(.i, .p, .o) {
#             robot("export", i = .i, header = sandwich_text(.p, '"'), e = .o)
#             out <- readr::read_tsv(
#                 .o,
#                 col_types = col_types,
#                 show_col_types = FALSE
#             )
#             tidy_sparql(out, tidy_what = "everything")
#         }
#     )
#
#     # output handling
#     if (is.null(output)) {
#         to_stdout <- TRUE
#         output <- temp_tsv
#     } else {
#         to_stdout <- FALSE
#     }
#
#     # handle output
#     if (isFALSE(to_stdout)) {
#         return(output)
#     }
#
#     if (q_type == "ask") {
#         ask_res <- readr::read_file(output)
#         out <- switch(ask_res, true = TRUE, false = FALSE, ask_res)
#     } else {
#         out <- readr::read_tsv(
#             output,
#             col_types = col_types,
#             show_col_types = FALSE
#         )
#         out <- tidy_sparql(out, tidy_what)
#     }
#     out
# }
# }


