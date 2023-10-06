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
