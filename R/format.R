#' Format DOIDs
#'
#' Convert valid DOIDs and/or bare numbers to a specified DOID format. Input
#' _may_ be tested to ensure it matches a valid DOID format but no attempt is
#' made to confirm bare numbers or DOIDs match actual diseases in the ontology.
#'
#' @inheritParams ID_predicates
#' @param as The format to convert the DOIDs to, as a string. All valid formats
#'     are possible options: "CURIE" (default), "URI", "obo_CURIE", "basename".
#' @param convert_bare Whether bare numbers should be converted to canonical
#'     DOIDs, `TRUE` or `FALSE` (default).
#' @param validate_input Whether to ensure only valid DOIDs are included in `x`,
#'     `TRUE` (default) or `FALSE`. When `FALSE`, non-DOID input will be
#'     returned unchanged.
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
#' # bare numbers can be converted to canonical DOIDs, if desired
#' w_bare <- c(x, "0050117")
#' format_doid(w_bare, convert_bare = TRUE)
#'
#' # non-DOIDs can be passed as input, if desired
#' mixed_input <- c(x, "random_text", "obo:SYMP_0000000", "0050117")
#' format_doid(mixed_input, validate_input = FALSE)
#' format_doid(mixed_input, convert_bare = TRUE, validate_input = FALSE)
#'
#' @export
format_doid <- function(x, as = "CURIE", convert_bare = FALSE,
                        validate_input = TRUE) {
    as <- match.arg(as, choices = c("URI", "CURIE", "obo_CURIE", "basename"))
    prefix <- switch(
        as,
        URI = "http://purl.obolibrary.org/obo/DOID_",
        CURIE = "DOID:",
        obo_CURIE = "obo:DOID_",
        basename = "DOID_"
    )

    if (convert_bare) {
        bare_number <- stringr::str_detect(x, "^[0-9]{1,7}$")
        if (validate_input) {
            assertthat::assert_that(all(is_valid_doid(x) | bare_number))
        }

        formatted <- dplyr::if_else(
            bare_number,
            paste0(prefix, x),
            stringr::str_replace(x, "^.*DOID[:_]", prefix)
        )
    } else {
        if (validate_input) {
            assertthat::assert_that(all(is_valid_doid(x)))
        }

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


#' Format Logical Axioms
#'
#' Format logical axioms in the style of Protege. Input axioms must be in OWL
#' functional syntax.
#'
#' @param x Complete logical axioms in OWL functional syntax, as a character
#'     vector.
#' @param property_df \[Optional\] A data.frame consisting of relevant annotation and object
#'     properties that may be used to make properties more readable in axioms.
#'     If provided, data.frame should include two columns:
#'
#' * `property`: Each property's URI.
#'
#' * `label`: Each property's `rdfs:label` (or equivalent).
#'
#' _See 'Formatting Options' for details._
#'
#' @param generify_obo Whether to make OBO ontology classes and properties
#'     generic, as `TRUE` or `FALSE` (default). _See 'Formatting Options' for_
#'     _details._
#' @param placeholders A set of 4 strings that will be used internally to
#'     preserve axiom parentheses `[1:2]`, phrase spacing `[3]`, and complete
#'     phrases `[4]`, as a character vector. These should not generally need to
#'     be edited and are only exposed in case of conflicts within axioms.
#' @param max_phrases The maximum number of phrases to update in an axiom,
#'     as an integer. This protects against an infinite while loops and would
#'     only need to be edited if one or more `Object*` OWL functional phrases
#'     are returned unformatted.
#'
#' @section Formatting Options:
#' `format_axiom()` will always rearrange equivalent class and subclass of
#' logical axioms from OWL functional syntax to a more readable form, similar
#' to Protege. However, classes and properties in axioms will be returned as
#' URIs or CURIEs (as formatted in `x`). This may be desirable for further
#' programmatic examination but will still be difficult for a human to read.
#' To make them more readable, the `property_df` and `generify_obo` arguments
#' can be used.
#'
#' `property_df` is used to replace OBO ontology property URIs or CURIEs
#' with namespace-prefixed labels: e.g. `obo:RO_0004026` would become
#' `RO:'disease has location'`. Non-OBO properties will not be modified.
#'
#' `generify_obo` replaces individual OBO URIs or CURIEs with either
#' namespace-prefixed types, e.g. `obo:UBERON_0001032` would become
#' `UBERON:anatomy`, or namespace only. This format simplifies axioms making
#' basic analysis of axiom patterns easier. _See the documentation for_
#' _[generify_obo()] for details._
#'
#' @export
format_axiom <- function(x, property_df = NULL, generify_obo = FALSE,
                         placeholders = c("<<<", ">>>", "%%%", "@@@"),
                         max_phrases = 15L) {

    out <- format_axiom_type(x)

    i <- 0
    while (any(has_object_stmt(out)) && i < max_phrases) {
        out <- format_inmost_object_phrase(
            out,
            placeholders = placeholders
        )
        i <- i + 1
    }

    if (!is.null(property_df)) {
        out <- label_properties(out, property_df)
    }
    if (isTRUE(generify_obo)) {
        out <- generify_obo(out)
    }

    # Replace placeholders used to protect phrases, parentheses and spaces.
    replacement <- purrr::set_names(
        c("\\(", "\\)", " "),
        nm = placeholders[1:3]
    )
    out <- stringr::str_replace_all(out, replacement)

    out
}

