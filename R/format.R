#' Format DOIDs
#'
#' Convert valid DOIDs and/or bare numbers to a specified DOID format. No
#' attempt is made to confirm bare numbers or DOIDs match existing diseases in
#' the ontology.
#'
#' @inheritParams is_valid_doid
#' @param as The format to convert the DOIDs to, as a string. All valid formats
#'     are possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".
#' @param allow_bare Whether bare numbers should be allowed as input, `TRUE`
#'     or `FALSE` (default).
#'
#' @examples
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "DOID:4",
#'     "obo:DOID_14566",
#'     "DOID_0040001"
#' )
#'
#' format_doid(x, as = "URI")
#' format_doid(x, as = "CURIE")
#' format_doid(x, as = "obo_CURIE")
#' format_doid(x, as = "basename")
#'
#' w_bare <- c(x, "0001816")
#' format_doid(w_bare, allow_bare = TRUE)
#'
#' @export
format_doid <- function(x, as = "CURIE", allow_bare = FALSE) {
    as <- match.arg(as, choices = c("URI", "CURIE", "obo_CURIE", "basename"))
    prefix <- switch(
        as,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    if (allow_bare) {
        bare_number <- stringr::str_detect(x, "^[0-9]{1,7}$")
        assertthat::assert_that(all(is_valid_doid(x) | bare_number))

        formatted <- dplyr::if_else(
            bare_number,
            paste0(prefix, x),
            stringr::str_replace(x, "^.*DOID[:_]", prefix)
        )
    } else {
        assertthat::assert_that(all(is_valid_doid(x)))
        formatted <- stringr::str_replace(x, "^.*DOID[:_]", prefix)
    }

    formatted
}



as_subtree_tidygraph <- function(x, top_node, limit_to_tree = TRUE,
                                 fill_subclasses = TRUE) {
    # keep all parent info in labels
    label_df <- DO.utils::collapse_col_flex(x, parent_id, parent_label)

    # exclude parents which are not subclasses of top_node (usually due to
    #   multi-parentage
    if (limit_to_tree) {
        df <- dplyr::filter(x, parent_id %in% id)
    } else {
        df <- x
    }

    if (fill_subclasses) {
        df <- fill_subclass(df)
    }

    # create tidygraph
    tg <- df %>%
        dplyr::select(id, parent_id) %>%
        tidygraph::as_tbl_graph() %>%
        # add labels
        tidygraph::left_join(
            label_df,
            by = c("name" = "id")
        )

    tg
}


pivot_subtree <- function(tg, top_node) {
    tg <- tidygraph::arrange(tg, label)
    r <- tg %>%
        tidygraph::activate("nodes") %>%
        tidygraph::as_tibble() %>%
        { .$name == top_node } %>%
        which()

    tg %>%
        tidygraph::activate("nodes") %>%
        dplyr::mutate(
            order = tidygraph::dfs_rank(
                root = r,
                mode = "in"
            ),
            parent = tidygraph::dfs_parent(
                root = r,
                mode = "in"
            ),
            dist = tidygraph::dfs_dist(
                root = r,
                mode = "in"
            ),
            insert = paste0("V", dist)
        ) %>%
        tidygraph::arrange(order) %>%
        tidygraph::as_tibble() %>%
        dplyr::select(
            parent_id, parent_label, name,
            tidyselect::everything()
        ) %>%
        tidyr::pivot_wider(
            names_from = insert,
            values_from = label
        )
}

fill_subclass <- function(df, debug = FALSE) {

    not_dup <- dplyr::filter(df, !duplicated(id))

    lvl <- 1
    res_n <- 1L
    new_rows <- list()

    while (res_n > 0) {
        if (lvl == 1) {
            new_rows[[lvl]] <- df %>%
                dplyr::filter(duplicated(id)) %>%
                dplyr::mutate(new_id = paste(id, lvl, sep = "-"))
        } else {
            new_rows[[lvl]] <- df %>%
                dplyr::filter(parent_id %in% new_rows[[lvl - 1]]$id) %>%
                dplyr::mutate(
                    new_id = paste(id, lvl, sep = "-"),
                    new_pid = paste(parent_id, lvl - 1, sep = "-")
                )
        }
        res_n <- nrow(new_rows[[lvl]])
        message("Round ", lvl, ": ", res_n, " IDs need to have children filled.")
        lvl <- lvl + 1
    }

    filled_df <- dplyr::bind_rows(no_dup, new_rows) %>%
        mutate(
            id = dplyr::if_else(is.na(new_id), id, new_id),
            parent_id = dplyr::if_else(is.na(new_pid), parent_id, new_pid)
        )

    if (!debug) {
        filled_df <- dplyr::select(filled_df, -new_id, -new_pid)
    }

    filled_df
}
