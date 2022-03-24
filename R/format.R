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


#' Format a Subtree
#'
#' Format a subtree, produced by [extract_subtree()], as a text-based tree
#' mirroring [disease-ontology.org](https://disease-ontology.org/).
#'
#' @param subtree_df A dataframe from [extract_subtree()].
#' @inheritParams extract_subtree
#'
#' @examples
#' \dontrun{
#'     do_owl <- {path_to_doid.owl_here}
#'     subtree <- extract_subtree(do_owl, "DOID:3070")
#'     st_formatted <- format_subtree(subtree, "DOID:3070")
#'     st_formatted
#' }
#'
#' @export
format_subtree <- function(subtree_df, top_node) {
    rlang::check_installed("tidygraph", reason = "to use `format_subtree()`")
    top_class <- format_doid(top_node, as = "CURIE")
    tg <- as_subtree_tidygraph(subtree_df, top_class)
    formatted <- pivot_subtree(tg, top_class)

    formatted
}


#' Convert Subtree DF to tidygraph
#'
#' Converts a subtree dataframe, from [extract_subtree()], to a tidygraph. This
#' is the first step in formatting a subtree as a text-based ontology tree
#' similar to the presentation on
#' [disease-ontology.org](https://disease-ontology.org/).
#'
#' @inheritParams format_subtree
#'
#' @keywords internal
as_subtree_tidygraph <- function(subtree_df, top_node) {
    # keep all parent info in labels
    label_df <- collapse_col_flex(
        subtree_df,
        "parent_id",
        "parent_label"
    )

    # exclude parents which are not subclasses of top_node (usually due to
    #   multi-parentage)
    df <- dplyr::filter(subtree_df, .data$parent_id %in% .data$id)

    # fill in subclasses where multi-parentage is within tree
    df <- fill_subclass(df)

    # create tidygraph
    tg <- df %>%
        dplyr::select(.data$id, .data$parent_id) %>%
        tidygraph::as_tbl_graph() %>%
        # fix needed for labels to match correctly
        tidygraph::activate("nodes") %>%
        dplyr::mutate(
            name = dplyr::if_else(
                .data$name %in% label_df$id,
                .data$name,
                stringr::str_remove(.data$name, "-[0-9]+$")
            )
        ) %>%
        # add labels
        tidygraph::left_join(
            label_df,
            by = c("name" = "id")
        )

    tg
}

#' Convert Subtree DF to tidygraph
#'
#' Converts a subtree tidygraph, from [as_subtree_tidygraph()], to a
#' dataframe with the tree hierarchically organized. This is the second step
#' in formatting a subtree as a text-based ontology tree similar to the
#' presentation on [disease-ontology.org](https://disease-ontology.org/).
#'
#' @param subtree_tg A subtree tidygraph from [as_subtree_tidygraph()].
#' @inheritParams format_subtree
#'
#' @keywords internal
pivot_subtree <- function(subtree_tg, top_node) {

    # ensure alphabetical order of classes to match disease-ontology.org
    tg <- tidygraph::arrange(subtree_tg, .data$label)

    # get top_node position in tidygraph; required for depth-first search fxns
    node_df <- tg %>%
        tidygraph::activate("nodes") %>%
        tidygraph::as_tibble()
    root_pos <- which(node_df$name == top_node)

    pivoted <- tg %>%
        tidygraph::activate("nodes") %>%
        tidygraph::mutate(
            order = tidygraph::dfs_rank(
                root = root_pos,
                mode = "in"
            ),
            dist = tidygraph::dfs_dist(
                root = root_pos,
                mode = "in"
            ),
            insert = paste0("V", .data$dist)
        ) %>%
        tidygraph::arrange(.data$order) %>%
        tidygraph::as_tibble() %>%
        # identify duplicates; useful when trying to identify changes over time
        dplyr::mutate(duplicated = all_duplicated(.data$name)) %>%
        # mv supporting info to left & tree to right
        dplyr::select(
            .data$parent_id, .data$parent_label, id = .data$name,
            dplyr::everything()
        ) %>%
        tidyr::pivot_wider(
            names_from = .data$insert,
            values_from = .data$label
        ) %>%
        # drop unnecessary info
        dplyr::select(-.data$order, -.data$dist)

    pivoted
}


#' Fill in Subclasses
#'
#' Fill in subclasses when a parent appears multiple times in a subtree. This
#' ensures that the subclasses appear each time their parent/superclass does.
#'
#' @inheritParams as_subtree_tidygraph
#'
#' @keywords internal
fill_subclass <- function(subtree_df) {

    not_dup <- dplyr::filter(subtree_df, !duplicated(.data$id))

    lvl <- 1
    res_n <- 1L
    new_rows <- list()

    while (res_n > 0) {
        if (lvl == 1) {
            new_rows[[lvl]] <- subtree_df %>%
                dplyr::filter(duplicated(.data$id)) %>%
                dplyr::mutate(id_new = paste(.data$id, lvl, sep = "-"))
        } else {
            new_rows[[lvl]] <- subtree_df %>%
                dplyr::filter(.data$parent_id %in% new_rows[[lvl - 1]]$id) %>%
                dplyr::mutate(
                    id_new = paste(.data$id, lvl, sep = "-"),
                    parent_id_new = paste(.data$parent_id, lvl - 1, sep = "-")
                )
        }
        res_n <- nrow(new_rows[[lvl]])
        if (res_n > 0) {
            message(
                "Round ", lvl, ": ", res_n, " IDs need to have children filled."
            )
        }
        lvl <- lvl + 1
    }

    filled_df <- dplyr::bind_rows(not_dup, new_rows) %>%
        dplyr::mutate(
            # currently using workaround for coalesce,
            #   https://github.com/tidyverse/funs/issues/54#issuecomment-892377998
            id = do.call(
                dplyr::coalesce,
                rev(dplyr::across(dplyr::starts_with("id")))
            ),
            parent_id = do.call(
                dplyr::coalesce,
                rev(dplyr::across(dplyr::starts_with("parent_id")))
            )
        ) %>%
        dplyr::select(id, label, parent_id, parent_label)

    filled_df
}
