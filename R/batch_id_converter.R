#' Convert IDs in Batches
#'
#' Batches inputs to convert any number of IDs using PubMed Centrals ID
#' converter API, via [rcrossref::id_converter].
#'
#' @inheritParams rcrossref::id_converter
#'
#' @export
batch_id_converter <- function(x, type = NULL, ...) {
    n <- length(x)

    if (n <= 200) {
        res <- rcrossref::id_converter(x)
    } else {
        pages <- seq(round_up(n / 200))
        s <- 200 * (pages - 1) + 1
        e <- 200 * pages
        e[-1] <- n

        res_list <- purrr::map2(
            .x = s,
            .y = e,
            function(.s, .e) {
                ids <- x[.s:.e]
                rcrossref::id_converter(ids, type = type, ...)
            }
        ) %>%
            purrr::transpose()# %>%
            #purrr::simplify_all()

        if (any(res_list$status != "ok")) {
            warning("id_converter experienced an error, full responses returned")
            return(res_list)
        }

        res <- dplyr::bind_rows(res_list$records)
    }

    res
}
