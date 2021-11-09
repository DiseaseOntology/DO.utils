#' Query a Virtuoso Graph
#'
#' Provides a thin wrapper around [virtuoso::vos_query()] so that a user can:
#' 1. Use a SPARQL file (.rq/.sparql).
#' 2. Specify a graph in the database without altering the query in the file.
#'
#' @inheritParams virtuoso::vos_query
#' @param query A SPARQL query OR the path to a SPARQL query file (.rq/.sparql),
#'     as a string.
#' @param uri The graph URI to be queried enclosed in angle brackets, as a
#'     string (example: "<graph_name>"). If not specified via this argument or
#'     in the query directly, all graphs in thebdatabase will be queried
#'     (default). `uri` is used _ONLY_ when `query` is a file and will _ONLY_
#'     limit the first query to the specified graph. For greater control, edit
#'     the SPARQL query directly and use [virtuoso::vos_query()].
#'
#' @export
virtuoso_query <- function(con, query, uri) {
    assertthat::assert_that(
        rlang::is_scalar_character(query)
    )

    if (stringr::str_detect(query, "\\.(rq|sparql)$")) {
        q_string <- readr::read_lines(query) %>%
            vctr_to_string(delim = "\n")

        if (!missing(uri)) {
            # validate uri
            # formatted correctly?
            assertthat::assert_that(
                stringr::str_detect(uri, "<.*>"),
                msg = "uri must be enclosed in angle brackets (eg. <graph_URI>)"
            )
            # in database?
            bare_uri <- stringr::str_trim(uri) %>%
                stringr::str_remove_all("^<|>$")
            if (!bare_uri %in% virtuoso::vos_list_graphs(con)$g) {
                rlang::abort(
                    c("Graph URI not found in database.", x = uri,
                    i = "Use virtuoso::vos_list_graph(con) to list graphs in db."
                    )
                )
            }

            # insert FROM statement
            where_regex <- "[Ww][Hh][Ee][Rr][Ee](.*\n*.*)+$"
            query_header <- stringr::str_remove(q_string, where_regex)
            query_where <- stringr::str_extract(q_string, where_regex)
            q_string <- glue::glue(
                "%query_header%FROM %uri%\n%query_where%",
                .open = "%", .close = "%"
            )
        }
        res <- virtuoso::vos_query(con, q_string) %>%
            tibble::as_tibble()
    } else {
        res <- virtuoso::vos_query(con, query) %>%
            tibble::as_tibble()
    }
    res
}
