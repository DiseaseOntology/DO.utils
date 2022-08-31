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
#' @family format_subtree() helpers
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
#' @family format_subtree() helpers
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
#' @family format_subtree() > as_subtree_tidygraph() helpers
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


# format_axiom() helpers --------------------------------------------------

#' Label OWL Properties (INTERNAL)
#'
#' Replaces OWL annotation and object property URI/CURIEs defined by OBO
#' ontologies with namespace-prefixed labels.
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() helpers
#' @noRd
label_properties <- function(x, property_df) {
    stopifnot(all(c("property", "label") %in% names(property_df)))

    obo_ns_pattern <- paste0("(", DO.utils::ns_prefix["obo"], "|obo:)")
    ns_label <- paste0(
        stringr::str_replace(
            property_df$property,
            ".+[/:]([A-Za-z]+)_[0-9]+",
            "\\1:"
        ),
        "'", property_df$label, "'"
    )
    prop_replacement <- purrr::set_names(
        ns_label,
        nm = stringr::str_replace(
            property_df$property,
            obo_ns_pattern,
            obo_ns_pattern
        )
    )

    out <- stringr::str_replace_all(x, prop_replacement)
    out
}

#' Generify OBO Classes/Properties (INTERNAL)
#'
#' [format_axiom()] internal helper that replaces URI/CURIEs defined by OBO
#' ontologies with namespace-prefixed descriptions/types, where defined
#' internally, or just the namespace. Will not modify non-OBO class URI/CURIEs.
#'
#' @inheritParams format_axiom
#'
#' @section Internally-defined OBO namespace-types:
#' The following namespaces have 'namespace:types' defined by this function:
#' * CL: 'CL:cell'
#' * CHEBI: 'CHEBI:chemical'
#' * DISDRIV: 'DISDRIV:disease_driver'
#' * DOID: 'DOID:disease'
#' * FOODON: 'FOODON:food_material'
#' * ECO: 'ECO:evidence'
#' * GENO: 'GENO:inheritance_pattern'
#' * NCBITaxon: 'NCBITaxon:organism'
#' * OMIM: 'OMIM:susceptibility'
#' * UBERON: 'UBERON:anatomy'
#' * UPHENO: 'UPHENO:phenotype'
#' * SO: 'SO:sequence'
#' * SYMP: 'SYMP:symptom'
#' * TRANS: 'TRANS:pathogen_transmission'
#'
#' Ontologies with multiple object types used by the DO (e.g. HP, onset or
#' phenotype), that are imported via DISDRIV (e.g. ExO), or used for their
#' properties (e.g. RO) are not defined internally and will be formatted
#' namespace only.
#' .
#'
#' @family format_axiom() helpers
#' @keywords internal
generify_obo <- function(x) {
    generic_ns_replacement <- c(
        "UBERON" = "UBERON:anatomy", "CL" = "CL:cell",
        "CHEBI" = "CHEBI:chemical", "DISDRIV" = "DISDRIV:disease_driver",
        "DOID" = "DOID:disease", "FOODON" = "FOODON:food_material",
        "ECO" = "ECO:evidence", "GENO" = "GENO:inheritance_pattern",
        "NCBITaxon" = "NCBITaxon:organism", "OMIM" = "OMIM:susceptibility",
        "UPHENO" = "UPHENO:phenotype", "SO" = "SO:sequence",
        "SYMP" = "SYMP:symptom", "TRANS" = "TRANS:pathogen_transmission"
    )

    obo_ns_pattern <- paste0(
        "(", DO.utils::ns_prefix["obo"], "|obo:)([A-Za-z]+)_[0-9]+"
    )
    out <- stringr::str_replace_all(x, obo_ns_pattern, "\\2")
    out <- stringr::str_replace_all(out, generic_ns_replacement)
    out
}

#' Formats Axiom Types (INTERNAL)
#'
#' Rearranges OWL equivalent class and subclass of types to human readable form,
#' patterned after Protege's 'Usage' tab.
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() helpers
#' @noRd
format_axiom_type <- function(x) {
    out <- stringr::str_replace(
        x,
        "(EquivalentClasses|SubClassOf)\\(([^ ]+) (.+)\\)",
        "\\2 \\1 \\3"
    )
    out <- stringr::str_replace(out, "EquivalentClasses", "EquivalentTo")

    out
}

#' Axiom Phrase Predicate (INTERNAL)
#'
#' Determines if OWL functional `Object.*` phrases exist in an axiom. Used to
#' determine when all phrases have been reformatted.
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() helpers
#' @noRd
has_object_stmt <- function(x) {
    stringr::str_detect(x, "ObjectSome|Object.+Cardinality|Object.+Of")
}

#' Format Inmost Axiom Phrase (INTERNAL)
#'
#' Extracts, formats, and replaces the inmost `Object.*` phrase of axioms.
#' Formatting removes the `Object.*` identifier from the phrase, which prevents
#' further modification. When used iteratively, this progressively formats
#' phrases.
#'
#' **NOTE:** This is the only format_axiom() helper that uses `placeholder[4]`.
#' `placeholder[1:3]` are passed along to internal functions.
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() helpers
#' @noRd
format_inmost_object_phrase <- function(x, placeholders) {
    extract <- stringr::str_extract(x, "Object[^(]+\\([^()]+\\)")
    phrase_placeholder <- placeholders[4]
    hold <- stringr::str_replace(
        x,
        "Object[^(]+\\([^()]+\\)",
        phrase_placeholder
    )
    mod <- format_object_phrase(extract, placeholders = placeholders[1:3])

    purrr::map2_chr(
        .x = hold,
        .y = mod,
        ~ stringr::str_replace(.x, phrase_placeholder, .y)
    )
}


#' Format Object Phrase (INTERNAL)
#'
#' Workhorse responsible for formatting axiom phrases. Currently, can format
#' 'some', 'IntersectionOf', 'UnionOf' and exact or minimum cardinality phrases.
#'
#' **NOTE:** `placeholder[1:3]` are passed along to internal functions.
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() > format_inmost_object_phrase() helpers
#' @noRd
format_object_phrase <- function(x, placeholders) {
    some <- stringr::str_detect(x, "ObjectSome")
    cardinal <- stringr::str_detect(x, "Object.+Cardinality")
    join <- stringr::str_detect(x, "Object.+Of")

    out <- dplyr::case_when(
        some ~ format_some_phrase(x, placeholders = placeholders),
        cardinal ~ format_cardinal_phrase(x, placeholders = placeholders),
        join ~ expand_join_phrase(x, placeholders = placeholders)
    )

    out
}

#' Format 'some' Phrase (INTERNAL)
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() > format_inmost_object_phrase() > format_object_phrase() helpers
#' @noRd
format_some_phrase <- function(x, placeholders) {
    replacement <- sandwich_text(
        paste("\\1", "some", "\\2", sep = placeholders[3]),
        placeholder = placeholders[1:2]
    )
    stringr::str_replace(
        x,
        "ObjectSomeValuesFrom\\(([^ ]+) (.+)\\)",
        replacement = replacement
    )
}

#' Format Min/Exact Cardinality Phrases (INTERNAL)
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() > format_inmost_object_phrase() > format_object_phrase() helpers
#' @noRd
format_cardinal_phrase <- function(x, placeholders) {
    replacement <- paste("\\3", "\\1", "\\2", "\\4", sep = placeholders[3])

    out <- stringr::str_replace(
        x,
        "Object([^C]+)Cardinality\\(([0-9]+) ([^ ]+) (.+)\\)",
        replacement = replacement
    )
    out <- sandwich_text(out, placeholders[1:2])
    out <- stringr::str_replace(
        out,
        sandwich_text("Exact", placeholders[3]),
        sandwich_text("Exactly", placeholders[3])
    )

    out
}

#' Expands Intersection/UnionOf Phrases (INTERNAL)
#'
#' @inheritParams format_axiom
#'
#' @family format_axiom() > format_inmost_object_phrase() > format_object_phrase() helpers
#' @noRd
expand_join_phrase <- function(x, placeholders) {
    type <- stringr::str_replace(x, "Object(.+)Of.*", "\\1")
    jxn <- dplyr::recode(
        type,
        Union = sandwich_text("or", placeholders[3]),
        Intersection = sandwich_text("and", placeholders[3])
    )
    out <- purrr::map2_chr(
        .x = x,
        .y = jxn,
        ~ stringr::str_replace_all(
            .x,
            c("Object.+Of\\(" = placeholders[1],
              " " = .y,
              "\\)" = placeholders[2]
            )
        )
    )

    out
}
