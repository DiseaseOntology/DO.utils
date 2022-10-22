
#' Curation Queue: DOIDs to split?
#'
#' Create a curation queue of DOIDs that may need to be split into multiple
#' diseases based on the fact that they have multiple xrefs from the same
#' source.
#'
#' @param .DOrepo The path to the local HumanDiseaseOntology repository or a
#' `pyDOID.repo.DOrepo` object created by [DOrepo()].
#' @param src The xref sources to include in the output identified by prefix
#' as a character vector or "all" (default) to include everything.
#'
#' @export
queue_xref_split <- function(.DOrepo, src = "all") {
    .class <- class(.DOrepo)
    src_lc <- stringr::str_to_lower(src)
    if ("pyDOID.repo.DOrepo" %in% .class) {
        repo <- .DOrepo
    } else if (length(.class) == 1 && .class == "character") {
        repo <- DOrepo(.DOrepo)
    } else {
        stop(
            ".DOrepo must be the path to a local copy of the ",
            "HumanDiseaseOntology repository or a pyDOID.repo.DOrepo object ",
            "created by DO.utils::DOrepo().",
            call. = FALSE
        )
    }

    xref <- repo$doid$query(
        system.file("sparql", "DO_xref.rq", package = "DO.utils")
    ) %>%
        tidy_sparql()

    to_split <- xref %>%
        dplyr::group_by(.data$ns) %>%
        dplyr::filter(all_duplicated(.data$id)) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(.data$id, .data$ns, .data$xref)

    if (!"all" %in% src_lc) {
        invalid <- !src_lc %in% stringr::str_to_lower(xref$ns)
        if (any(invalid)) {
            valid <- unique_to_string(xref$ns, delim = ", ", sort = TRUE)
            invalid_src <- paste0(
                sandwich_text(src[invalid], "'"),
                collapse = ", "
            )
            rlang::abort(
                c(
                    paste0(
                        "`src` must be an xref prefix or 'all', not: ",
                        invalid_src
                    ),
                  "i" = paste0("Valid prefixes: ", valid)
                )
            )
        }
        to_split <- dplyr::filter(
            to_split,
            stringr::str_to_lower(.data$ns) %in% src_lc
        )
    }
    to_split
}
