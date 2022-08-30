# format_subtree() helpers ------------------------------------------------

#' Convert Subtree DF to tidygraph (INTERNAL)
#'
#' Converts a subtree dataframe, from [extract_subtree()], to a tidygraph. This
#' is the first step in formatting a subtree as a text-based ontology tree
#' similar to the presentation on
#' [disease-ontology.org](https://disease-ontology.org/).
#'
#' @inheritParams format_subtree
#'
#' @noRd
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

#' Convert Subtree DF to tidygraph (INTERNAL)
#'
#' Converts a subtree tidygraph, from [as_subtree_tidygraph()], to a
#' dataframe with the tree hierarchically organized. This is the second step
#' in formatting a subtree as a text-based ontology tree similar to the
#' presentation on [disease-ontology.org](https://disease-ontology.org/).
#'
#' @param subtree_tg A subtree tidygraph from [as_subtree_tidygraph()].
#' @inheritParams format_subtree
#'
#' @noRd
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


#' Fill in Subclasses (INTERNAL)
#'
#' Fill in subclasses when a parent appears multiple times in a subtree. This
#' ensures that the subclasses appear each time their parent/superclass does.
#'
#' @inheritParams as_subtree_tidygraph
#'
#' @noRd
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
        dplyr::select(.data$id, .data$label, .data$parent_id, .data$parent_label)

    filled_df
}
