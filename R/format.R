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
#' @section Caution:
#' Be extra cautious when using `format_doid()` with `validate_input = FALSE` as
#' unexpected text conversion may occur.
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
#' @family format IDs
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


#' Format OBO Foundry IDs
#'
#' Convert valid OBO Foundry ontology IDs to a specified format. Input
#' _may_ be tested to ensure it matches a valid OBO ID format but no attempt is
#' made to confirm IDs match actual terms in any OBO Foundry ontology.
#'
#' @inheritParams ID_predicates
#' @param as The format to convert the OBO IDs to, as a string. The following
#'     formats are possible options:
#'
#' * `"CURIE"` (default)
#'
#' * `"URI"`
#'
#' * `"bracketed_URI"`: e.g. `"<http://purl.obolibrary.org/obo/CL_0000066>"`
#'
#' * `"ns_lui"`: namespace with local unique identifier (preserves separator).
#'
#' * `"ns"`: namespace of ontology only
#'
#' As valid OBO formats, the first three formats may be modified repeatedly
#' by `format_obo()`. The 'ns' formats, on the other hand, are not valid OBO
#' formats and cannot be formatted again by `format_obo()`.
#'
#' @param validate_input Whether to ensure only valid OBO IDs are included in
#'     `x`,`TRUE` (default) or `FALSE`. When `FALSE`, non-OBO ID input will
#'     _most likely_ be returned unchanged.
#'
#' @section Caution:
#' Be extra cautious when using `format_obo()` with `validate_input = FALSE` as
#' unexpected text conversion may occur.
#'
#' @examples
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "<http://purl.obolibrary.org/obo/CL_0000066>",
#'     "obo:SYMP_0000000",
#'     "obo:so#has_origin"
#' )
#'
#' # reversible
#' format_obo(x, as = "CURIE")
#' format_obo(x, as = "URI")
#' format_obo(x, as = "bracketed_URI")
#'
#' # irreversible
#' format_obo(x, as = "ns_lui")
#' format_obo(x, as = "ns")
#'
#' # non-OBO IDs can be passed as input with caution, if desired
#' mixed_input <- c(x, "random_text", "0050117", "obo:SYMP:0000000")
#' format_obo(mixed_input, validate_input = FALSE)
#'
#' @family format IDs
#' @export
format_obo <- function(x, as = "CURIE", validate_input = TRUE) {
    as <- match.arg(
        as,
        choices = c("CURIE", "URI", "bracketed_URI", "ns_lui", "ns")
    )

    if (validate_input) {
        assertthat::assert_that(all(is_valid_obo(x)))
    }

    obo_pattern <- "^.*obo[/:]([A-Za-z_]+)(_[[:alnum:]_]+)>?$|^.*obo[/:]([A-Za-z_]+#)([[:alnum:]_]+)>?$"
    obo_replacement <- switch(
        as,
        URI = "http://purl.obolibrary.org/obo/\\1\\2\\3\\4",
        CURIE = "obo:\\1\\2\\3\\4",
        bracketed_URI = "<http://purl.obolibrary.org/obo/\\1\\2\\3\\4>",
        ns_lui = "\\1\\2\\3\\4",
        ns = "\\1\\3"
    )
    formatted <- stringr::str_replace(x, obo_pattern, obo_replacement)

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


#' Format URLs as Hyperlinks
#'
#' Formats URLs as hyperlinks for Google Sheets, Excel, or html.
#'
#' @param url One or more URLs, as a character vector.
#' @param txt The text to display for each link, as a character vector.
#' @param as What format to use for the hyperlink, as a string; one of "gs"
#' (Google Sheet), "xlsx" (Excel), or "html".
#' @param ... One or more name-value pairs of html `<a>`
#' [attributes](https://www.w3schools.com/tags/tag_a.asp).
#' @param preserve_NA Whether to preserve `NA` in output, as a boolean. `FALSE`
#' will result in hyperlinks built from `NA` values in `url` (almost
#' certainly not desired).
#'
#' @section Excel Note:
#' Use the `openxlsx` pkg to write data with hyperlinks to Excel.
#'
#' @examples
#' format_hyperlink("https://www.google.com", "Google", "gs")
#' format_hyperlink("https://www.google.com", "Google", "xlsx")
#' format_hyperlink("https://www.google.com", "Google", "html")
#'
#' # html with `<a>` attributes
#' format_hyperlink(
#'     "https://www.google.com",
#'     "Google",
#'     "html",
#'     target = "_blank",
#'     rel = "external"
#' )
#'
#' @export
format_hyperlink <- function(url, txt, as, ..., preserve_NA = TRUE) {
    as <- match.arg(as, c("gs", "xlsx", "html"))
    if (length(txt) != length(url)) {
        rlang::abort("`txt` must be the same length as `url`")
    }

    if (as %in% c("gs", "xlsx")) {
        formula <- as.character(glue::glue('=HYPERLINK("{url}", "{txt}")'))

        if (as == "gs") {
            formatted <- googlesheets4::gs4_formula(formula)
        } else {
            formatted <- formula
            class(formatted) <- "formula"
        }
    }

    if (as == "html") {
        attr <- list(...)
        if (length(attr) > 0) {
            unnamed <- names(attr) == ""
            if (any(unnamed)) {
                rlang::abort(
                    c(
                        "All hyperlink attributes in `...` must be named.",
                        purrr::set_names(attr[unnamed], nm = rep("x", sum(unnamed)))
                    )
                )
            }

            html_attr <- paste0(
                " ", names(attr), "=", sandwich_text(attr, '"'),
                collapse = ""
            )
        } else {
            html_attr <- NULL
        }

        formatted <- glue::glue(
            '<a href="{url}"{html_attr}>{txt}</a>',
            .null = ""
        )
        formatted <- as.character(formatted)
    }

    if (preserve_NA) {
        formatted[is.na(url)] <- NA
    }

    formatted
}
