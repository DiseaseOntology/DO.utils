#' Convert IDs in Batches
#'
#' Batches inputs to convert any number of IDs using PubMed Centrals ID
#' converter API, via [rcrossref::id_converter]. Input cannot have missing
#' values and _should_ be unique.
#'
#' @inheritParams rcrossref::id_converter
#'
#' @export
batch_id_converter <- function(x, type = NULL, ...) {
    uniq <-
    n <- length(x)

    if (n <= 200) {
        res <- rcrossref::id_converter(x)
        check_id_conv_status(res)
        out <- res$records
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
            purrr::transpose()

        check_id_conv_status(res_list)
        out <- dplyr::bind_rows(res_list$records)
    }

    out
}


check_id_conv_status <- function(res) {
    if (any(res$status != "ok")) {
        warning(
            "ID converter API request returned an error, full responses returned for debugging",
            call. = FALSE
        )
        return(res)
    }
}
