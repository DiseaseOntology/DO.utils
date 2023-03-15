#' Tests for Write Access
#'
#' Acts as a minimal wrapper around [file.access(mode = 2)](base::file.access)
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
#' @param graph A [tidygraph](tidygraph::tidygraph-package) or
#'     [igraph](https://cran.r-project.org/package=igraph) object.
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
    file.path(getwd(), file)
}
