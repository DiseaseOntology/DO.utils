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
#' @inheritParams robot
#'
#' @returns
#' If `output` is specified, the path to the output file with the query result.
#' Otherwise, the query result (ASK as boolean or SELECT as `tibble`).
#'
#' @seealso [robot()] for underlying implementation.
#'
#' @export
robot_query <- function(input, query, output = NULL, ...,
                        tidy_what = "nothing", col_types = NULL,
                        .robot_path = NULL) {
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
        robot(
            "query",
            i = input,
            update = query,
            o = output,
            ...,
            .robot_path = .robot_path
        )
    } else {
        robot(
            "query",
            i = input,
            query = query,
            output,
            ...,
            .robot_path = .robot_path
        )
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


#' Execute Multiple SPARQL Queries with ROBOT
#'
#' Wrapper for `robot("query", ...)` that accepts one `input` but multiple
#' queries, and optionally multiple output files.
#'
#' @param input The path to an RDF/OWL file recognized by ROBOT, as a string.
#' @param query The text for or path to one or more valid SPARQL queries (`ASK`,
#' `SELECT`, `CONSTRUCT`, or `UPDATE`) as a character vector.
#' @param output The path(s) where output will be written or `NA`, as a
#' character vector. If `NULL` (default), or for each `NA` data will be loaded
#' directly. NOTE: `output` is required for `UPDATE` and `CONSTRUCT` queries.
#' @param ... Additional arguments to
#' [ROBOT query](http://robot.obolibrary.org/query) formatted as described in
#' [DO.utils::robot()].
#' @inheritParams tidy_sparql
#' @inheritParams readr::read_tsv
#' @inheritParams robot
#'
#' @returns
#' If `output` is specified, the path(s) to the output file(s) with the query
#' result(s). Otherwise, the query result(s) as a list. ASK results will be
#' boolean, while SELECT results will be as [tibble](tibble::tibble)s.
#'
#' @seealso [robot()] for underlying implementation.
#'
#' @export
robot_mquery <- function(input, query, output = NA, ...,
                        tidy_what = "nothing", col_types = NULL,
                        .robot_path = NULL) {
    stopifnot("`input` must be a string" = is_string(input))
    query_info <- check_query(query)
    output_info <- check_query_output(output, query_info)




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
        robot(
            "query",
            i = input,
            update = query,
            o = output,
            ...,
            .robot_path = .robot_path
        )
    } else {
        robot(
            "query",
            i = input,
            query = query,
            output,
            ...,
            .robot_path = .robot_path
        )
    }

    # handle output
    if (isFALSE(to_stdout)) {
        return(output)
    }

    if (q_type == "ask") {
        ask_res <- readr::read_file(output)
        out <- switch(ask_res,
            true = TRUE,
            false = FALSE,
            ask_res
        )
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


# robot_mquery helpers -------------------------------------------------------

#' Check Queries
#'
#' Check if queries are files or text, write those input as text to file, and
#' determine query types.
#'
#' @inheritParams robot_mquery
#' @returns Query file paths and types, as a named character vector.
#' @noRd
check_query <- function(query) {
    stopifnot("`query` must be a character vector" = is.character(query))

    query_is_file <- file.exists(query)

    # capture query file location, write those input as text (required by ROBOT)
    temp_query_file <- purrr::map_chr(
        1:sum(!query_is_file), ~ tempfile(fileext = ".sparql")
    )
    out <- query
    out[!query_is_file] <- purrr::map2_chr(
        query[!query_is_file],
        temp_query_file,
         ~ { readr::write_lines(.x, .y) ; .y }
    )

    query_txt <- query
    query_txt[query_is_file] <- purrr::map_chr(
        query[query_is_file],
        readr::read_file
    )

    # determine query type (SELECT = default, but not in search since it can be
    # used in other queries)
    q_type <- stringr::str_extract(
        query_txt,
        stringr::regex(
            "\\b(insert|delete|construct|ask)\\b",
            ignore_case = TRUE
        )
    )[1]
    q_type <- stringr::str_to_lower(q_type)
    q_type[is.na(q_type)] <- "select"
    q_type <- switch(q_type,
        insert = "update",
        delete = "update",
        q_type
    )
    names(out) <- q_type
    out
}

check_query_output <- function(output, query_info) {
    q_type <- names(query_info)
    q_req_output <- q_type %in% c("update", "construct")
    output_req <- output[q_req_output]
    # if (any(q_req_output)) {
    #     rlang::abort(
    #         "`output` is required for each CONSTRUCT or UPDATE (INSERT/DELETE) `query`",
    #         purrr::set_names(

    #         )
    # }
    if (!is.null(output) && length(output) != length(query_info)) {
        rlang::abort("`output` must be the same length as `query`")
    }

    if (q_type %in% c("update", "construct") && is.null(output)) {
        rlang::abort("`output` is required for CONSTRUCT or UPDATE (INSERT/DELETE) `query`")
    }

    # capture output file location
    temp_output_file <- purrr::map_chr(
        1:sum(!output_is_file), ~ tempfile(fileext = ".tsv")
    )
    out <- output
    out[!output_is_file] <- temp_output_file

    out
}