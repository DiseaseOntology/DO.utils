#' Elucidate the Data in an Object
#'
#' Elucidates (makes clear) what data is in a given object. `elucidate()` is
#' similar to [utils::str()] and [dplyr::glimpse()] but provides more detail on
#' the nature of the data in an object.
#'
#' @param x An R object with a defined `elucidate()` method.
#' @param type The type of report to create, as a string; either "basic"
#' (default) or "full". See description of the Value returned by individual
#' methods for more details.
#' @param print Whether to print a basic elucidation to standard out
#' (default: `TRUE`).
#' @param ... Additional arguments for methods. Not currently used.
#'
#' @export
elucidate <- function(x, type = "basic", print = TRUE, ...) {
    UseMethod("elucidate")
}

#' @rdname elucidate
#'
#' @returns `omim_inventory` method:
#'
#' - For `type = "basic"`, a single `tibble` of OMIM and DOID statistical
#' information including the total number of terms for each, the number of OMIM
#' terms present/absent in DO, the number of deprecated DO terms, and the number
#' of terms with one-to-many matches (in either direction).
#' - For `type = "full"`, the "basic" statistical `tibble` and additional
#' `tibble`'s containing the full records for the following:
#'     - `doid_deprecated`,
#'     - `omim_to_many`: Results where an OMIM ID corresponds to multiple
#' DOIDs, excluding skos broad/narrow/related matches.
#'     - `doid_to_many`): Results where a DOID corresponds to multiple
#' OMIM IDs, excluding skos broad/narrow/related matches.
#'
#' @export
elucidate.omim_inventory <- function(x, type = "basic", print = TRUE,
                                         ...) {
    dep <- dplyr::filter(x, isTRUE(.data$do_dep))
    omim_to_many <- dplyr::filter(
        x,
        .data$multimaps %in% c("omim_to_doid", "both_ways")
    )
    doid_to_many <- dplyr::filter(
        x,
        .data$multimaps %in% c("doid_to_omim", "both_ways")
    )
    basic <- tibble::tribble(
        ~ "report", ~ "n",
        "omim_total", dplyr::n_distinct(x$omim),
        "omim_absent", dplyr::n_distinct(x$omim[is.na(x$doid)]),
        "omim_present", dplyr::n_distinct(x$omim[!is.na(x$doid)]),
        "omim_to_many", dplyr::n_distinct(omim_to_many$omim),
        "doid_total", dplyr::n_distinct(x$doid, na.rm = TRUE),
        "doid_deprecated", dplyr::n_distinct(dep$doid),
        "doid_to_many", dplyr::n_distinct(doid_to_many$doid)
    )
    class(basic) <- c("oieb", class(basic)) # omim_inventory_elucidation_basic

    if (type == "full") {
        out <- list(
            stats = basic,
            dep = dep,
            omim_to_many = omim_to_many,
            doid_to_many = doid_to_many
        )
    } else {
        out <- basic
    }

    if (print) {
        print(basic)
    }

    invisible(out)
}
