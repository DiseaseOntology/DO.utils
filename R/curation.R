#' Create a Curation Template
#'
#' Create a curation template in a Google Sheet, optionally including data.
#'
#' @inheritParams googlesheets4::range_write
#' @param .data Data to add to the curation sheet. If `NULL` (default), an empty
#' curation sheet will be created.
#' @param sheet (OPTIONAL) The sheet name, as a string. If `NULL` (default), the
#' sheet name will default to "curation-" with today's date appended (formatted
#' as "%Y%m%d"; see [format.Date()]).
#'
#' @returns The Google Sheet info (`ss`), as a [googlesheets4::sheets_id].
#'
#' @export
curation_template <- function(.data = NULL, ss = NULL, sheet = NULL, ...) {
    UseMethod("curation_template", .data)
}

#' @param nrow The number of rows to create in the curation template when
#' `.data = NULL` (default: `50`).
#'
#' @export
#' @rdname curation_template
curation_template.NULL <- function(ss = NULL, sheet = NULL, ..., nrow = 50) {
  val <- rep(NA, nrow)

  # inspired by https://stackoverflow.com/a/60495352/6938922
  cur_df <- tibble::as_tibble(rlang::rep_named(curation_cols, list(val)))

  class(cur_df) <- c("curation_template", class(cur_df))
  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  if (is.null(ss)) ss <- gs_info
  set_curation_validation(cur_df, ss, sheet)

  invisible(gs_info)
}

#' @export
curation_template.obo_data <- function(.data, ss = NULL, sheet = NULL, ...,
                                       n_max = 20) {
    cur_df <- .data |>
        # need smarter indexing... I think
        dplyr::mutate(
            index = dplyr::dense_rank(paste0(.data$predicate, .data$value)),
            .by = "id",
            .before = "id"
        ) |>
        dplyr::mutate(
            predicate = dplyr::if_else(
                !is.na(.data$axiom_predicate) & .data$axiom_predicate == "oboInOwl:hasSynonymType",
                paste0(.data$predicate, "-", .data$axiom_value),
                .data$predicate
            ),
            axiom_predicate = dplyr::if_else(
                !is.na(.data$axiom_predicate) & .data$axiom_predicate != "oboInOwl:hasSynonymType",
                paste0(.data$predicate, "-", .data$axiom_predicate),
                NA_character_
            ),
            # removes axiom value where predicate is updated (redundant)
            axiom_value = dplyr::if_else(
                is.na(.data$axiom_predicate),
                NA_character_,
                .data$axiom_value
            )
        ) |>
        tidyr::pivot_longer(
            cols = -c("index", "id"),
            names_to = ".value",
            names_prefix = "^axiom_",
            values_drop_na = TRUE
        ) |>
        dplyr::arrange(
            .data$id,
            .data$index,
            stringr::str_length(.data$predicate)
        ) |>
        dplyr::rename(data_type = "predicate", "curation_notes" = "extra") |>
        # for now, just remove index --> need to use for sorting at some point
        dplyr::select(-"index") |>
        unique() |>
        # collapse_col(value) |> # does nothing... probably don't want to collapse
        dplyr::mutate(
            data_type = dplyr::coalesce(
                .sparql_dt_motif[.data$data_type],
                .data$data_type
            ),
            id = dplyr::if_else(duplicated(.data$id), NA_character_, .data$id)
        ) |>
        append_empty_col(curation_cols, order = TRUE)


  class(cur_df) <- c("curation_template", class(cur_df))
  if (is.null(sheet)) sheet <- paste0("curation-", format(Sys.Date(), "%Y%m%d"))
  gs_info <- googlesheets4::write_sheet(cur_df, ss, sheet)

  if (is.null(ss)) ss <- gs_info
  set_curation_validation(cur_df, ss, sheet)

  invisible(gs_info)
}


# helpers --------------------------------------------------------------------

# define expected columns for curation template (in order)
curation_cols <- c(
  "id", "data_type", "value", "status", "curation_notes", "links",
  "status_notes"
)

#' Curation 'Status'
#'
#' Values used to establish `status` data validation in Google Sheets
#' [curation templates][curation_template()].
#'
#' * `retain`: data already in ontology that should be kept; this is the default
#' `status` for existing data when creating a [curation_template()]
#'
#' * `add`: new data that should be added
#'
#' * `remove`: existing ontology data that should be removed
#'
#' * `exclude`: data relevant to the ontology that should be actively excluded
#' (e.g. an incorrect mapping) -- details should be included in `status_notes`
#'
#' * `ignore`: data not for active inclusion or exclusion that should be ignored
#' (e.g. dubious synonyms, incomplete curation data)
#'
#' * `restore`: data that was removed from the ontology and should be added back
#'
#' @keywords internal
curation_status <- c("retain", "add", "remove", "exclude", "ignore", "restore")


#' Set Data Validation for Curation Templates
set_curation_validation <- function(cur_df, ss, sheet) {
    # add data_type validation
    dt_range <- spreadsheet_range(cur_df, "data_type", sheet = sheet)
    range_add_dropdown(ss, dt_range, values = .curation_opts$data_type)

    # add status validation
    status_range <- spreadsheet_range(cur_df, "status", sheet = sheet)
    range_add_dropdown(ss, status_range, values = curation_status)

    # freeze first two columns
    googlesheets4::with_gs4_quiet(
        googlesheets4:::sheet_freeze(ss, sheet = sheet, ncol = 2)
    )
}

#' Calculate a Spreadsheet Range
#'
#' Calculate a range for a spreadsheet program (Google Sheets or Excel).
#'
#' @inheritParams curation_template
#' @param .data A tibble.
#' @param .col The column to use for the range, as a string.
#' @param rows (OPTIONAL) The rows to use for the range, either as a continous
#' integer vector or as a string (i.e. "1:10"). If `NULL` (default), the entire
#' column will be used.
#' @param n_header The number of header rows to skip (default: `1`).
#'
#' @keywords internal
spreadsheet_range <- function(.data, .col, sheet = NULL, rows = NULL,
                                 n_header = 1) {
  col_letter <- LETTERS[which(names(.data) == .col)]
  if (length(col_letter) != 1) {
    rlang::abort("Exactly one column must be specified in `.col`")
  }

  if (is.null(rows)) {
    row_ends <- c(1, nrow(.data)) + n_header
  } else if (is.numeric(rows)) {
    # check one continuous range
    collapsed_range <- to_range(rows, sep = c(",", ":"))
    if (stringr::str_count(collapsed_range, "[,:]") > 1) {
      rlang::abort(
        c("`rows` must be one continuous range", x = collapsed_range)
      )
    }
    row_ends <- c(rows[1], tail(rows, 1)) + n_header
  } else {
    row_ends <- as.integer(stringr::str_split(row_ends, ":")[[1]]) + n_header
  }

  range <- paste0(col_letter, row_ends, collapse = ":")
  if (!is.null(sheet)) {
    range <- paste0(sheet, "!", range)
  }
  range
}

#' Ensure label first
cur_order <- function(cur_df) {
    cur_df |>
        dplyr::arrange(.data$id) |>
        group_by(.data$id) |>
        arrange(dplyr::desc(.data$data_type == "label"), .by_group = TRUE)
}


#' Generate hierarchical path column for OBO/OWL tabular data
#'
#' @param .df Data frame with columns: id, predicate, value, label, data_type
#' @param parent_predicate Predicate used for parent relationships (default: "rdfs:subClassOf")
#' @param label_predicate Predicate used for labels (default: "rdfs:label")
#' @param word_match_pct Percentage of words to match for label similarity (default: 0.5)
#' @return Data frame with new 'path' column
order_cur_classes <- function(.df, parent_predicate = "rdfs:subClassOf",
                              label_predicate = "rdfs:label",
                              word_match_pct = 0.5) {
    # Get labels for each id
    labels <- .df |>
        dplyr::filter(.data$predicate == .env$label_predicate) |>
        dplyr::select(id, label = value)
    # Get parent relationships
    parents <- .df %>%
        dplyr::filter(predicate == parent_predicate) %>%
        dplyr::select(id, parent_id = value)
    # Build child-to-parent map
    parent_map <- split(parents$parent_id, parents$id)
    # Build label map
    label_map <- purrr::set_names(labels$label, labels$id)
    # Helper: get namespace
    get_ns <- function(x) strsplit(x, ":")[[1]][1]
    # Helper: word match
    word_match <- function(a, b) {
        wa <- str_split(a, "\\s+")[[1]]
        wb <- str_split(b, "\\s+")[[1]]
        sum(wa %in% wb) / length(wa)
    }
    # Build graph
    all_ids <- unique(.df$id)
    children_map <- purrr::map(all_ids, function(pid) {
        which_child <- parents$id[parents$parent_id == pid]
        which_child
    })
    names(children_map) <- all_ids
    # Helper: find best parent for multiple parents
    choose_parent <- function(child_id, parent_ids) {
        # 4a: parent-child among parents
        parent_children <- sapply(parent_ids, function(pid) children_map[[pid]])
        lowest_child <- parent_ids[which(sapply(parent_ids, function(pid) child_id %in% children_map[[pid]]))]
        if (length(lowest_child) > 0) return(lowest_child[1])
        # 4c: most siblings
        sibling_counts <- sapply(parent_ids, function(pid) length(children_map[[pid]]))
        max_sib <- parent_ids[which.max(sibling_counts)]
        if (length(max_sib) == 1) return(max_sib)
        # 4b: label similarity
        child_label <- label_map[[child_id]]
        match_scores <- sapply(parent_ids, function(pid) word_match(child_label, label_map[[pid]]))
        best_match <- parent_ids[which.max(match_scores)]
        if (max(match_scores) >= word_match_pct) return(best_match[1])
        # 4d: alphabetical
        parent_labels <- label_map[parent_ids]
        return(parent_ids[order(parent_labels)][1])
    }
    # Build hierarchy order
    # 1. Group by namespace
    ns_map <- sapply(all_ids, get_ns)
    ns_order <- sort(unique(ns_map))
    # 2. Isolated entities
    has_parent <- all_ids %in% parents$id
    has_child <- all_ids %in% parents$parent_id
    isolated <- all_ids[!has_parent & !has_child]
    # 3. Build order recursively
    ordered_ids <- c()
    add_entity <- function(eid, depth = 0, visited = character()) {
        if (eid %in% visited) return()
        visited <- c(visited, eid)
        ordered_ids <<- c(ordered_ids, eid)
        # Find children
        children <- parents$id[parents$parent_id == eid]
        # For each child, if multiple parents, choose one
        for (cid in children) {
            c_parents <- parents$parent_id[parents$id == cid]
            if (length(c_parents) > 1) {
                best_parent <- choose_parent(cid, c_parents)
                if (best_parent != eid) next
            }
            add_entity(cid, depth + 1, visited)
        }
    }
    # For each namespace, add isolated entities first, then build hierarchy
    for (ns in ns_order) {
        ns_ids <- all_ids[ns_map == ns]
        ns_isolated <- intersect(isolated, ns_ids)
        # Sort isolated by label
        ns_isolated <- ns_isolated[order(label_map[ns_isolated])]
        ordered_ids <- c(ordered_ids, ns_isolated)
        # Add roots (no parent in set)
        roots <- ns_ids[!ns_ids %in% parents$id]
        for (root in roots) {
            add_entity(root)
        }
    }
    # Build path column
    id_depth <- setNames(rep(0, length(all_ids)), all_ids)
    id_path <- setNames(rep("", length(all_ids)), all_ids)
    for (eid in ordered_ids) {
        # Build path from parent chain
        path <- label_map[[eid]]
        depth <- 0
        pid <- parents$parent_id[parents$id == eid]
        while (length(pid) > 0 && pid %in% ordered_ids) {
            path <- paste(label_map[[pid]], ">", path)
            depth <- depth + 1
            pid <- parents$parent_id[parents$id == pid]
        }
        id_depth[eid] <- depth
        id_path[eid] <- paste0(strrep("-", depth), " ", path)
    }
    # Merge path column into output
    out <- .df %>%
        left_join(labels, by = "id") %>%
        mutate(path = id_path[id])
    return(out)
}


    # This script orders rows in a data.frame so that for each parent-child relationship,
    # parents appear before their children, recursively, using tidygraph.
    # If an id has multiple parents, it is assigned to the parent with the most siblings.

    # Example input data.frame
    # Replace this with your actual data
    # rel_df <- dplyr::filter(cur_df, data_type == "parent id") |>
    #     dplyr::select("id", "parent" = "value") |>
    #     unique() |>
    #     dplyr::mutate(ppresent = .data$parent %in% .data$id) |>
    #     dplyr::mutate(pp_n = sum(ppresent), .by = "id")
    #
    # isolated <- dplyr::filter(rel_df, pp_n == 0)$id
    # single <- dplyr::filter(rel_df, pp_n == 1)
    # |>
    #
    #     dplyr::mutate(
    #         p_n = dplyr::n(.data$parent),
    #         pp_n = dplyr::.by = "id") |>
    #     dplyr::mutate(
    #         siblings = dplyr::n_distinct(.data$id),
    #         .by = "parent"
    #     ) |>
    #
    #     dplyr::summarize(
    #         total = dplyr::n_distinct(.data$value),
    #         present =
    #             .by = "id")
    # single <-
    # id_label <- dplyr::filter(cur_df, data_type == "label") |>
    #     with(purrr::set_names(value, id))
    # key <- dplyr::filter(cur_df, data_type == "parent id") |>
    #     dplyr::select("id", parent_id = "value") |>
    #     dplyr::mutate(
    #         label = id_label[id],
    #         parent_label = id_label[parent_id]
    #     )
    #     dplyr::left_join(
    #         dplyr::filter(cur_df, data_type == "label") |>
    #             dplyr::select("parent id" = "id", "parent value" = "value"),
    #         by = c("value" = "parent id")
    #     ) |>
    #     dplyr::summarize(
    #         label = .data$value[.data$data_type == "label"],
    #         parent = list(
    #             purrr::set_names(
    #                 .data$value[.data$data_type == "parent id"],
    #                 .data$`parent value`[.data$data_type == "parent id"]
    #         ),
    #         .by = "id"
    #     )
    #     tidyr::pivot_wider(
    #         id_cols = "id",
    #         names_from = "data_type",
    #         values_from = "value",
    #         values_fn = list
    #     ) |>
    #     tidyr::unnest(cols = "label") |>
    #     dplyr::mutate(
    #         parent = purrr::map(
    #             .data[["parent id"]],
    #             ~ purrr::set_names(
    #                 .data$label[dplyr::pick("id").x],
    #
    #         )
    #     )
    #     dplyr::left_join(
    #         dplyr::filter(cur_df, data_type == "label") |>
    #             dplyr::select(
    #                 `parent id` = "id",
    #                 `parent label` = "value"
    #             ),
    #         by = "parent id"
    #     ) |>
    #     unique()
    #
    #
    #     dplyr::mutate(
    #         status = dplyr::case_when(
    #             tidygraph::node_is_isolated() ~ "isolated",
    #             tidygraph::node_is_sink() ~ "sink",
    #             tidygraph::node_is_source()  ~ "source",
    #             TRUE ~ "intermediate"
    #         )
    #     ) |>
    #     tidygraph::activate("nodes") |>
    #
    #
    #
    #
    #
    # df <- tibble(
    #     id = c("A", "B", "C", "D", "E", "F", "G"),
    #     data_type = c("label", "label", "label", "label", "label", "label", "label"),
    #     value = c("Alpha", "Bravo", "Charlie", "Delta", "Echo", "Foxtrot", "Golf")
    # ) %>%
    #     bind_rows(
    #         tibble(
    #             id = c("B", "C", "D", "E", "F", "G"),
    #             data_type = "parent id",
    #             value = c("A", "A|B", "B", "C", "C|D", "")
    #         )
    #     )
    #
    #
    # # 1. Parse parent relationships
    # parent_map <- df %>%
    #     dplyr::filter(data_type == "parent id") %>%
    #     mutate(parent_ids = str_split(value, "\\|")) %>%
    #     select(id, parent_ids)
    #
    # # 2. Build edge list (parent-child pairs)
    # edges <- parent_map %>%
    #     unnest(parent_ids) %>%
    #     dplyr::filter(parent_ids != "") %>%
    #     select(parent = parent_ids, child = id)
    #
    # # 3. Assign each child to the parent with the most siblings
    # siblings_count <- edges %>%
    #     group_by(parent) %>%
    #     summarise(siblings = n(), .groups = "drop")
    #
    # edges <- edges %>%
    #     left_join(siblings_count, by = "parent") %>%
    #     group_by(child) %>%
    #     slice_max(order_by = siblings, n = 1, with_ties = FALSE) %>%
    #     ungroup() %>%
    #     select(parent, child)
    #
    # # 4. Build tidygraph object
    # all_ids <- unique(c(df$id, edges$parent, edges$child))
    # nodes <- tibble(name = all_ids)
    # graph <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
    #
    # # 5. Find roots (nodes that are never children)
    # roots <- setdiff(edges$parent, edges$child)
    # if (length(roots) == 0) {
    #     # If no roots, treat all nodes with no parent as roots
    #     roots <- setdiff(all_ids, edges$child)
    # }
    #
    # # 6. Recursive ordering using depth-first search from each root
    # dfs_order <- function(graph, root) {
    #     ord <- igraph::dfs(graph, root = which(V(graph)$name == root), order = TRUE)$order
    #     V(graph)$name[ord]
    # }
    # ordered_ids <- unlist(lapply(roots, dfs_order, graph = graph))
    #
    # # 7. Add any ids not in the graph at the end
    # remaining_ids <- setdiff(df$id, ordered_ids)
    # final_order <- c(ordered_ids, remaining_ids)
    #
    # # 8. Reorder the data.frame
    # df_ordered <- df %>%
    #     mutate(id = factor(id, levels = final_order)) %>%
    #     arrange(id)
    #
    # # Print result
    # print(df_ordered)
# }

# a <- extract_obo_data(
#     "../Ontologies/HumanDiseaseOntology/src/ontology/doid-edit.owl",
#     include_anon = F,
#     id = c(
#         "UBERON:0002072", # 3 parents
#             "UBERON:0013754", # parent
#             "UBERON:0004923", # grandparent --> no parent
#             "UBERON:0004120", # other --> no parent
#         "UBERON:0000979", # 4 parents, 3 present ???similar label or parent-grandparent???
#             "UBERON:0015004", # parent similar label
#             "UBERON:0003608", # parent
#             "UBERON:0002495", # grandparent
#             "UBERON:0004251", # other
#         "CHEBI:30879", # 2 parents, 2 present
#             "FOODON:00001579", # parent similar label
#             "CHEBI:33822", # other
#         "DOID:7665", "DOID:5550", "DOID:5351", # 2 parents, 2 present
#         "DOID:6517", "DOID:4211", # 2 parents, 1 present,
#         "DOID:0111088", "DOID:13636", # 1 parent, 1 present
#         "DOID:5154", "DOID:8659", # 2 children, 1 parent
#         "DOID:934", # parent
#         "DOID:0050117", # parent-sibling
#         "DOID:104" # grandparent
#         )
#     )


#' Order Curation Classes Hierarchically
#'
#' Orders ontology classes hierarchically for curation, grouping all rows
#' belonging to each class together and adding a visual path column showing
#' hierarchy with `-` prefixes for depth.
#'
#' @param .df Data frame with columns: id, data_type, value
#' @param parent_predicate Predicate used for parent relationships (default:
#'     "rdfs:subClassOf")
#' @param label_predicate Predicate used for labels (default: "rdfs:label")
#' @param word_match_pct Percentage of words to match for label similarity
#'     (default: 0.5)
#'
#' @returns
#' Data frame reordered by hierarchy with a new 'path' column as the first
#' column, showing hierarchical relationships using `-` prefixes and label paths.
#'
#' @section Ordering Rules:
#' 1. Groups are sorted by namespace alphabetically
#' 2. Isolated entities (no parent or child) are placed at the top within
#'    namespace groups, sorted alphabetically by label
#' 3. Entities with a single parent in the set are placed directly below that
#'    parent
#' 4. For entities with multiple parents in the set:
#'    * (a) If parent-child relationships exist among parents, choose the
#'          lowest child as the parent
#'    * (c) If not, choose the parent that yields the most siblings
#'    * (b) If still tied, check if any parent's label matches the entity's
#'          label by majority of words (≥50% by default)
#'    * (d) If none apply, choose the parent with the first alphabetical label
#' 5. Where no hierarchical relationships define the order, sort alphabetically
#'    by label
#'
#' @export
# order_cur_classes <- function(.df,
#                               parent_predicate = "rdfs:subClassOf",
#                               label_predicate = "rdfs:label",
#                               word_match_pct = 0.5) {
#     # Get labels for each id
#     labels <- .df |>
#         dplyr::filter(.data$data_type == "rdfs:label") |>
#         dplyr::select("id", label = "value") |>
#         dplyr::distinct()
#
#     # Get parent relationships (parents that are in the dataset)
#     rel_df <- .df |>
#         dplyr::filter(
#             .data$data_type == "rdfs:subClassOf",
#             is_curie(.data$value) # excludes anonymous classes
#         ) |>
#         # dplyr::filter(.data$parent_id %in% labels$id)
#         dplyr::mutate(sibs = list(unique(.data$id)), .by = "value") |>
#         dplyr::summarize(
#             parents = list(unique(.data$value)),
#             sibs = list(Reduce(base::union, .data$sibs)),
#             .by = "id"
#         ) |>
#             dplyr::left_join(labels, by = "id")
#
#         # dplyr::mutate(
#         #     child_present = .data$id %in% .data$parent_id,
#         #     parent_present = .data$parent_id %in% .data$id,
#         #     grandparent = dplyr::coalesce(.data$id[.data$parent_id == .data$id], NA_character_),
#         #     type = dplyr::case_when(
#         #         !.data$child_present & !.data$parent_present & sibs_n == 1 ~ "isolated",
#         #         !.data$child_present & !.data$parent_present ~ "orphan",
#         #         !.data$child_present ~ "source",
#         #         !.data$parent_present & sibs_n > 1 ~ "orphan/hier",
#         #         !.data$parent_present ~ "sink",
#         #         TRUE ~ "intermediate"
#         #     ),
#         #     lvl = identify_lvl(dplyr::pick("id", "parent_id", "sibs"))
#         # )
#
#     construct_paths <- function(rel_df) {
#         ids <- rel_df$id
#         out <- list(
#             lvl = rep(0, length(ids)),
#             path = rel_df$label
#         )
#         i <- 0
#         while(length(ids) > 0 | i < cutoff) {
#             id_at_lvl <- paste0()
#
#
#             keep <- id_at_lvl & rel_df$parent_id %in% rel_df$sibs
#
#             out[keep] <- paste0(strrep("-", i), " ", out[keep])
#             ids <- ids[!ids %in% rel_df$id[keep]]
#             rel_df <- rel_df[!rel_df$id %in% rel_df$id[keep], ]
#             i <- i + 1
#         }
#
#     }
#
#     identify_lvl <- function(.df, cutoff < 5) {
#         id <- .df$id
#         out <- list()
#         while (length(id) > 0 || i <= cutoff) {
#             id_at_lvl <- !.df$id %in% .df$pid
#             keep <- id_at_lvl & .df$pid %in% .df$sibs)
#
#
#             out[[i]] <- id[]
#             # if no children --> at this level
#             id_lvl <- id_map[names()]
#             id_map <-
#         }
#         lvl0 <- dplyr::if_else(!pid %in% id, 0, 1)
#         id
#         child <- !id %in% pid
#         parent <- pid %in% id
#     }
#
#
#     hgroup <- rel_df |>
#         dplyr::filter(
#             .data$type %in% c("source", "sink", "intermediate", "orphan/hier")
#         ) |>
#         dplyr::mutate(
#             parent_preferred = dplyr::case_when(
#                 .data$parent_id
#             )
#             .by = "id"
#         )
#             group = .data$parent_id
#
#     # Build label map
#     label_map <- purrr::set_names(labels$label, labels$id)
#
#     # Create lowercase label map for case-insensitive sorting
#     label_map_lower <- purrr::map_chr(label_map, tolower)
#
#     # Helper: get namespace
#     get_ns <- function(x) {
#         sapply(x, function(i) strsplit(i, ":")[[1]][1])
#     }
#
#     # Helper: word match percentage
#     word_match <- function(a, b) {
#         wa <- stringr::str_split(a, "\\s+")[[1]]
#         wb <- stringr::str_split(b, "\\s+")[[1]]
#         matches <- sum(wa %in% wb)
#         matches / length(wa)
#     }
#
#     # Build graph structures
#     all_ids <- unique(.df$id)
#
#     # Build children map (id -> vector of child IDs)
#     children_map <- purrr::map(
#         purrr::set_names(all_ids),
#         function(pid) {
#             parents$id[parents$parent_id == pid]
#         }
#     )
#
#     # Helper: find best parent for multiple parents
#     choose_parent <- function(child_id, parent_ids) {
#         if (length(parent_ids) == 1) {
#             return(parent_ids)
#         }
#
#         # 4a: If parent-child relationships exist among parents, choose lowest
#         #     child (i.e., a parent that is itself a child of another parent)
#         parent_is_child <- parent_ids[parent_ids %in% parents$id]
#         parents_of_these <- parents$parent_id[parents$id %in% parent_is_child]
#         # Lowest child = one that is also in parent_ids
#         lowest_children <- parent_is_child[parent_is_child %in% parents_of_these]
#         if (length(lowest_children) > 0) {
#             # If multiple, pick first alphabetically by label (case-insensitive)
#             if (length(lowest_children) > 1) {
#                 lowest_children <- lowest_children[order(label_map_lower[lowest_children])]
#             }
#             return(lowest_children[1])
#         }
#
#         # 4c: Choose parent with most siblings (most children)
#         sibling_counts <- sapply(
#             parent_ids,
#             function(pid) length(children_map[[pid]])
#         )
#         max_sib_count <- max(sibling_counts)
#         max_sib_parents <- parent_ids[sibling_counts == max_sib_count]
#         if (length(max_sib_parents) == 1) {
#             return(max_sib_parents)
#         }
#
#         # 4b: Label similarity - check if any parent label matches by word majority
#         child_label <- label_map[[child_id]]
#         match_scores <- sapply(
#             max_sib_parents,
#             function(pid) word_match(child_label, label_map[[pid]])
#         )
#         best_match_score <- max(match_scores)
#         if (best_match_score >= word_match_pct) {
#             best_matches <- max_sib_parents[match_scores == best_match_score]
#             if (length(best_matches) > 1) {
#                 best_matches <- best_matches[order(label_map_lower[best_matches])]
#             }
#             return(best_matches[1])
#         }
#
#         # 4d: Alphabetical by parent label (case-insensitive)
#         parent_labels_lower <- label_map_lower[max_sib_parents]
#         sorted_parents <- max_sib_parents[order(parent_labels_lower)]
#         return(sorted_parents[1])
#     }
#
#     # Build hierarchy order
#     ns_map <- get_ns(all_ids)
#
#     # Identify entities by their relationships IN THE DATASET
#     has_parent_in_dataset <- all_ids %in% parents$id
#     has_child_in_dataset <- all_ids %in% parents$parent_id
#
#     # Isolated: no parent or child in the dataset
#     truly_isolated <- all_ids[!has_parent_in_dataset & !has_child_in_dataset]
#
#     # Get ALL parent relationships (including those not in dataset) for orphan detection
#     all_parents <- .df |>
#         dplyr::filter(.data$data_type == .env$parent_predicate) |>
#         dplyr::select(.data$id, parent_id = .data$value)
#
#     # Orphan siblings: 2+ entities in dataset that share a parent not in dataset
#     # (they can have children in the dataset - they're still part of hierarchical groups)
#     entities_with_external_parents <- all_ids[
#         all_ids %in% all_parents$id &  # has parent in full ontology
#             !has_parent_in_dataset          # but not in dataset
#     ]
#
#     orphan_groups <- list()
#     orphan_placeholders <- character()
#
#     if (length(entities_with_external_parents) > 0) {
#         # Group by their missing parent (only IRI parents, not anonymous classes)
#         orphan_parents_df <- all_parents |>
#             dplyr::filter(
#                 .data$id %in% entities_with_external_parents,
#                 !.data$parent_id %in% all_ids,  # parent not in dataset
#                 !grepl("'|some|and|or|not", .data$parent_id)  # exclude anonymous classes
#             ) |>
#             dplyr::select("id", missing_parent = "parent_id")
#
#         if (nrow(orphan_parents_df) > 0) {
#             orphan_by_parent <- split(orphan_parents_df$id, orphan_parents_df$missing_parent)
#
#             # Only create orphan groups for parents with 2+ children in dataset
#             for (missing_parent in names(orphan_by_parent)) {
#                 orphan_children <- orphan_by_parent[[missing_parent]]
#                 if (length(orphan_children) >= 2) {
#                     orphan_groups[[missing_parent]] <- orphan_children
#                     orphan_placeholders <- c(orphan_placeholders, missing_parent)
#                 }
#             }
#         }
#     }
#
#     # Save all orphan siblings before filtering (for removing from isolated list)
#     all_orphan_siblings <- unlist(orphan_groups, use.names = FALSE)
#
#     # Remove orphan siblings from isolated list (they have their own groups)
#     truly_isolated <- setdiff(truly_isolated, all_orphan_siblings)
#
#     # Identify hierarchical groups (connected components)
#     # Build adjacency for connected components
#     get_connected_group <- function(start_id, max_iter = 1000) {
#         group <- character()
#         to_process <- start_id
#         iter <- 0
#
#         while (length(to_process) > 0 && iter < max_iter) {
#             iter <- iter + 1
#             current <- to_process[1]
#             to_process <- to_process[-1]
#
#             if (current %in% group) {
#                 next
#             }
#             group <- c(group, current)
#
#             # Add parents and children
#             entity_parents <- parents$parent_id[parents$id == current]
#             entity_children <- parents$id[parents$parent_id == current]
#
#             new_ids <- c(entity_parents, entity_children)
#             new_ids <- new_ids[!new_ids %in% group & !new_ids %in% to_process]
#             to_process <- c(to_process, new_ids)
#         }
#
#         if (iter >= max_iter) {
#             warning("Max iterations reached for group starting with ", start_id, call. = FALSE)
#         }
#
#         group
#     }
#
#     # Find all hierarchical groups (exclude only truly isolated entities)
#     non_isolated <- setdiff(all_ids, truly_isolated)
#     groups <- list()
#     processed <- character()
#
#     for (eid in non_isolated) {
#         if (eid %in% processed) {
#             next
#         }
#         group <- get_connected_group(eid)
#         if (length(group) > 1) {
#             groups <- c(groups, list(group))
#         }
#         processed <- c(processed, group)
#     }
#
#     # Now filter orphan_groups: only keep groups where orphan siblings are NOT in hierarchical groups
#     # But keep ALL orphan_placeholders for path building
#     all_orphan_placeholders <- names(orphan_groups)
#     pure_orphan_groups <- list()
#     for (missing_parent in names(orphan_groups)) {
#         orphan_children <- orphan_groups[[missing_parent]]
#         # Check if any orphan child is in a hierarchical group
#         in_hierarchical <- any(sapply(groups, function(grp) any(orphan_children %in% grp)))
#         if (!in_hierarchical) {
#             # This is a pure orphan group (no hierarchy among siblings)
#             pure_orphan_groups[[missing_parent]] <- orphan_children
#         }
#     }
#     # Update orphan_groups to only pure orphan groups (for group type classification)
#     # But keep all placeholders for path building
#     orphan_groups <- pure_orphan_groups
#     orphan_placeholders <- all_orphan_placeholders
#
#     # Assign each group to a namespace (most common, or first alphabetically if tied)
#     group_namespaces <- sapply(groups, function(grp) {
#         grp_ns <- get_ns(grp)
#         ns_counts <- table(grp_ns)
#         max_count <- max(ns_counts)
#         most_common <- names(ns_counts)[ns_counts == max_count]
#         # If tied, pick first alphabetically
#         sort(most_common)[1]
#     })
#
#     # Get namespace order
#     ns_order <- sort(unique(c(ns_map, group_namespaces)))
#
#     # Build ordered list of IDs using child-up approach
#     ordered_ids <- character()
#     visited <- character()
#
#     # Helper to add entity and its ancestors (working up the chain)
#     # Returns TRUE if entity was added, FALSE if already visited
#     add_entity_with_ancestors <- function(eid) {
#         if (eid %in% visited) {
#             return(FALSE)
#         }
#
#         # Get parents of this entity
#         entity_parents <- parents$parent_id[parents$id == eid]
#
#         if (length(entity_parents) == 0) {
#             # No parent - this is a root, just add it
#             visited <<- c(visited, eid)
#             ordered_ids <<- c(ordered_ids, eid)
#             return(TRUE)
#         }
#
#         # Determine chosen parent vs other parents
#         if (length(entity_parents) > 1) {
#             chosen_parent <- choose_parent(eid, entity_parents)
#             other_parents <- setdiff(entity_parents, chosen_parent)
#         } else {
#             chosen_parent <- entity_parents[1]
#             other_parents <- character()
#         }
#
#         # Option A: Other parents appear ABOVE (before) the chosen parent
#         # First, add other parents (sorted alphabetically, case-insensitive)
#         if (length(other_parents) > 0) {
#             other_parents <- other_parents[!other_parents %in% visited]
#             if (length(other_parents) > 0) {
#                 other_parents_sorted <- other_parents[order(label_map_lower[other_parents])]
#                 for (op in other_parents_sorted) {
#                     add_entity_with_ancestors(op)
#                 }
#             }
#         }
#
#         # Then, recursively add chosen parent and its ancestors
#         add_entity_with_ancestors(chosen_parent)
#
#         # Finally, add this entity
#         visited <<- c(visited, eid)
#         ordered_ids <<- c(ordered_ids, eid)
#         return(TRUE)
#     }
#
#     # Process each namespace: sort all groups together alphabetically
#     # Groups include: truly isolated (single entities), orphan sibling groups, and hierarchical groups
#     for (ns in ns_order) {
#         # Collect all groups for this namespace
#         ns_groups <- list()
#
#         # Add truly isolated entities as single-entity groups
#         ns_truly_isolated <- truly_isolated[ns_map[truly_isolated] == ns]
#         if (length(ns_truly_isolated) > 0) {
#             for (iso in ns_truly_isolated) {
#                 ns_groups[[paste0("isolated_", iso)]] <- list(
#                     type = "isolated",
#                     entities = iso,
#                     sort_label = label_map_lower[[iso]]
#                 )
#             }
#         }
#
#         # Add orphan sibling groups (only those WITHOUT hierarchical relationships among siblings)
#         for (missing_parent in names(orphan_groups)) {
#             orphan_children <- orphan_groups[[missing_parent]]
#             orphan_ns <- get_ns(orphan_children)
#             # Assign group to most common namespace among children
#             orphan_ns_counts <- table(orphan_ns)
#             max_count <- max(orphan_ns_counts)
#             most_common_ns <- names(orphan_ns_counts)[orphan_ns_counts == max_count]
#             group_ns <- sort(most_common_ns)[1]
#
#             if (group_ns == ns) {
#                 orphan_children_sorted <- orphan_children[order(label_map_lower[orphan_children])]
#                 ns_groups[[paste0("orphan_", missing_parent)]] <- list(
#                     type = "orphan",
#                     placeholder = missing_parent,
#                     entities = orphan_children_sorted,
#                     sort_label = label_map_lower[[orphan_children_sorted[1]]]
#                 )
#             }
#         }
#
#         # Add hierarchical groups
#         ns_group_indices <- which(group_namespaces == ns)
#         if (length(ns_group_indices) > 0) {
#             for (gi in ns_group_indices) {
#                 grp <- groups[[gi]]
#                 # Find roots in this group (no parent in dataset)
#                 grp_roots <- grp[!grp %in% parents$id]
#                 grp_roots_sorted <- grp_roots[order(label_map_lower[grp_roots])]
#                 first_root <- grp_roots_sorted[1]
#
#                 ns_groups[[paste0("hierarchy_", gi)]] <- list(
#                     type = "hierarchy",
#                     group_index = gi,
#                     entities = grp,
#                     sort_label = label_map_lower[[first_root]]
#                 )
#             }
#         }
#
#         # Sort groups by type first (isolated, orphan, hierarchy), then by label within each type
#         if (length(ns_groups) > 0) {
#             # Create sorting keys: type gets priority, then label
#             type_priority <- c("isolated" = 1, "orphan" = 2, "hierarchy" = 3)
#             group_type_orders <- sapply(ns_groups, function(g) type_priority[[g$type]])
#             group_labels <- sapply(ns_groups, function(g) g$sort_label)
#
#             # Sort by type first, then by label
#             group_order <- order(group_type_orders, group_labels)
#
#             # Process each group in sorted order
#             for (idx in group_order) {
#                 grp_info <- ns_groups[[idx]]
#
#                 if (grp_info$type == "isolated") {
#                     # Single isolated entity
#                     iso <- grp_info$entities
#                     if (!iso %in% visited) {
#                         visited <- c(visited, iso)
#                         ordered_ids <- c(ordered_ids, iso)
#                     }
#
#                 } else if (grp_info$type == "orphan") {
#                     # Orphan sibling group: add placeholder then children
#                     ordered_ids <- c(ordered_ids, grp_info$placeholder)
#                     for (orphan_child in grp_info$entities) {
#                         if (!orphan_child %in% visited) {
#                             visited <- c(visited, orphan_child)
#                             ordered_ids <- c(ordered_ids, orphan_child)
#                         }
#                     }
#
#                 } else if (grp_info$type == "hierarchy") {
#                     # Hierarchical group: find leaves and work up
#                     grp <- grp_info$entities
#                     grp_leaves <- grp[!grp %in% parents$parent_id]
#
#                     # If no leaves (cycle?), use all unvisited entities
#                     if (length(grp_leaves) == 0) {
#                         grp_leaves <- grp[!grp %in% visited]
#                     }
#
#                     # Sort leaves alphabetically (case-insensitive) and process
#                     if (length(grp_leaves) > 0) {
#                         grp_leaves_sorted <- grp_leaves[order(label_map_lower[grp_leaves])]
#                         for (leaf in grp_leaves_sorted) {
#                             if (!leaf %in% visited) {
#                                 add_entity_with_ancestors(leaf)
#                             }
#                         }
#                     }
#                 }
#             }
#         }
#     }
#
#     # Build path for each ID (including placeholders)
#     path_list <- purrr::map(
#         purrr::set_names(ordered_ids),
#         function(eid) {
#             # Check if this is a placeholder for missing parent
#             if (eid %in% orphan_placeholders) {
#                 return(paste0("[", eid, "] -- not present"))
#             }
#
#             # Check if this is an orphan child (has missing parent)
#             is_orphan <- FALSE
#             missing_parent_id <- NA_character_
#             for (mp in names(orphan_groups)) {
#                 if (eid %in% orphan_groups[[mp]]) {
#                     is_orphan <- TRUE
#                     missing_parent_id <- mp
#                     break
#                 }
#             }
#
#             if (is_orphan) {
#                 # Orphan child: path = "- [missing_parent] > label"
#                 path_str <- paste0("[", missing_parent_id, "] > ", label_map[[eid]])
#                 final_path <- paste0("- ", path_str)
#                 return(final_path)
#             }
#
#             # Regular entity: get all immediate parents
#             all_parents <- parents$parent_id[parents$id == eid]
#
#             if (length(all_parents) == 0) {
#                 # No parent - just the entity label
#                 return(label_map[[eid]])
#             }
#
#             # Determine the preferred parent
#             preferred_parent <- if (length(all_parents) > 1) {
#                 choose_parent(eid, all_parents)
#             } else {
#                 all_parents[1]
#             }
#
#             # Build a path for each parent, storing with parent ID for sorting
#             path_info <- purrr::map(
#                 all_parents,
#                 function(parent_id) {
#                     # Build chain by following ancestors up from this parent
#                     chain <- character()
#                     current <- parent_id
#
#                     # Traverse up the hierarchy
#                     max_iterations <- 100
#                     iter_count <- 0
#
#                     while (current %in% parents$id && iter_count < max_iterations) {
#                         iter_count <- iter_count + 1
#                         parent_options <- parents$parent_id[parents$id == current]
#                         if (length(parent_options) == 0) {
#                             break
#                         }
#                         # For intermediate ancestors, choose best parent
#                         chosen_parent <- choose_parent(current, parent_options)
#                         # Prevent infinite loop
#                         if (chosen_parent == current || chosen_parent %in% chain) {
#                             break
#                         }
#                         chain <- c(chosen_parent, chain)
#                         current <- chosen_parent
#                     }
#
#                     # Add the immediate parent to the chain
#                     chain <- c(chain, parent_id)
#
#                     # Build label path for this parent
#                     path_labels <- c(label_map[chain], label_map[[eid]])
#                     path_str <- paste(path_labels, collapse = " > ")
#                     depth <- length(chain)
#
#                     # Add depth prefix
#                     prefix <- paste0(strrep("-", depth), " ")
#                     final_path <- paste0(prefix, path_str)
#
#                     list(
#                         parent_id = parent_id,
#                         path = final_path,
#                         is_preferred = (parent_id == preferred_parent)
#                     )
#                 }
#             )
#
#             # Sort paths: non-preferred first (alphabetically), preferred last
#             non_preferred <- purrr::keep(path_info, ~ !.x$is_preferred)
#             preferred <- purrr::keep(path_info, ~ .x$is_preferred)
#
#             # Sort non-preferred alphabetically by path (case-insensitive)
#             if (length(non_preferred) > 0) {
#                 non_preferred_paths <- purrr::map_chr(non_preferred, ~ .x$path)
#                 non_preferred_order <- order(tolower(non_preferred_paths))
#                 non_preferred <- non_preferred[non_preferred_order]
#             }
#
#             # Combine: non-preferred first, then preferred
#             sorted_path_info <- c(non_preferred, preferred)
#             all_paths <- purrr::map_chr(sorted_path_info, ~ .x$path)
#
#             # Return all paths joined by newline
#             paste(all_paths, collapse = "\n")
#         }
#     )
#
#     # Create path data frame
#     path_df <- tibble::tibble(
#         id = ordered_ids,
#         path = unlist(path_list)
#     )
#
#     # Separate placeholders from real entities
#     real_entities <- setdiff(ordered_ids, orphan_placeholders)
#
#     # Within-entity sorting: label first, then by index
#     out <- .df |>
#         dplyr::mutate(
#             # Create sort keys
#             id_factor = factor(.data$id, levels = real_entities),
#             label_first = dplyr::if_else(
#                 .data$data_type == .env$label_predicate,
#                 0L,
#                 1L
#             )
#         ) |>
#         dplyr::arrange(.data$id_factor, .data$label_first, .data$index) |>
#         dplyr::select(-"id_factor", -"label_first") |>
#         dplyr::left_join(path_df, by = "id") |>
#         dplyr::relocate("path", .before = dplyr::everything())
#
#     # Add placeholder rows for missing parents
#     if (length(orphan_placeholders) > 0) {
#         placeholder_rows <- tibble::tibble(
#             path = character(),
#             index = integer(),
#             id = character(),
#             data_type = character(),
#             value = character()
#         )
#
#         for (placeholder in orphan_placeholders) {
#             placeholder_row <- tibble::tibble(
#                 path = path_list[[placeholder]],
#                 index = NA_integer_,
#                 id = paste0("[", placeholder, "]"),
#                 data_type = NA_character_,
#                 value = NA_character_
#             )
#             placeholder_rows <- dplyr::bind_rows(placeholder_rows, placeholder_row)
#         }
#
#         # Combine and resort based on ordered_ids
#         # Create a lookup for sorting: placeholders use their position in ordered_ids
#         sort_lookup <- purrr::set_names(seq_along(ordered_ids), ordered_ids)
#
#         out_with_placeholders <- dplyr::bind_rows(out, placeholder_rows) |>
#             dplyr::mutate(
#                 # For placeholders (bracketed IDs), extract the ID without brackets
#                 sort_key = dplyr::if_else(
#                     stringr::str_detect(.data$id, "^\\[.*\\]$"),
#                     stringr::str_extract(.data$id, "(?<=\\[)[^\\]]+(?=\\])"),
#                     .data$id
#                 ),
#                 sort_order = sort_lookup[.data$sort_key]
#             ) |>
#             dplyr::arrange(.data$sort_order) |>
#             dplyr::select(-"sort_key", -"sort_order")
#
#         return(out_with_placeholders)
#     }
#
#     out
# }
#
# # Helper function to write output safely with proper quoting
# write_ordered_output <- function(result, file) {
#     readr::write_tsv(result, file, quote = "needed", escape = "double")
# }

