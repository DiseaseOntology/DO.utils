#' Format OBO Foundry IDs
#'
#' @description
#' Convert valid OBO Foundry ontology IDs to a specified format. Input
#' _may_ be tested to ensure it matches a valid OBO ID format but no attempt is
#' made to confirm IDs match actual terms in any OBO Foundry ontology.
#'
#' `format_doid` is a convenience function focused solely on DOIDs.
#'
#' @param as The format to convert the OBO IDs to, as a string.  One of:
#' `"curie"` (default), `"obo_curie"` (e.g. `obo:DOID_4`,
#' `obo:doid#DO_rare_slim`), `"uri"`, `"<uri>"` (URI surrounded by angle
#' brackets), `"ns.lui"` (i.e. an OBO CURIE without `obo:`).
#' @param validate Whether to ensure only valid OBO IDs are included in
#' `x`, `TRUE` (default) or `FALSE`. When `FALSE`, non-OBO ID input will
#' be handled differently depending on `as` (see `Non-OBO CURIE/URIs` section).
#' @inheritParams obo_ID_predicates
#'
#' @section Note on `ns_type`:
#' The `ns_type` argument affects both validation _AND_ formatting.
#'
#' @section Non-OBO CURIE/URIs:
#' Be extra cautious when using `format_obo()` with non-OBO Foundry CURIE/URIs
#' (i.e. `validate = FALSE`). In an effort to allow meaningful
#' pass-through in some situations, non-OBO CURIE/URIs are returned unchanged,
#' except when `as = "uri"` or `"<uri>"`, in which case the original input is
#' assumed to be URIs and is either stripped or surrounded by angle brackets.
#'
#' @examples
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "<http://purl.obolibrary.org/obo/CL_0000066>",
#'     "obo:SYMP_0000000",
#'     "obo:so#has_origin",
#'     "DOID:4"
#' )
#'
#' format_obo(x, as = "curie")
#' format_obo(x, as = "uri")
#' format_obo(x, as = "<uri>")
#' format_obo(x, as = "obo_curie")
#' format_obo(x, as = "ns.lui")
#'
#' # ns.lui input can be validated, if explicitly specified
#' try(format_obo(c(x, "DOID_0001816"), allow = "standard"))
#' format_obo(c(x, "DOID_0001816"), allow = c("standard", "ns.lui"))
#'
#' # non-OBO IDs can be passed as input with caution, if desired
#' mixed_input <- c(
#'     x, "rdfs:label", "<http://xmlns.com/foaf/0.1/Person>",
#'     "random_text", "0050117", "obo:SYMP:0000000"
#' )
#' format_obo(mixed_input, validate = FALSE)
#'
#' # ns_type will influence output, even when validate = FALSE
#' # e.g. only obo:so#has_origin (property) is converted to a CURIE (so:has_origin)
#' format_obo(x, as = "curie", validate = FALSE, ns_type = "prop")
#'
#' @export
format_obo <- function(x, as = "curie", validate = TRUE,
                       allow = "standard", ns_type = "obo") {
    as <- match.arg(
        as,
        choices = c("curie", "obo_curie", "uri", "<uri>", "ns.lui")
    )

    if (validate) {
        valid_obo <- is_valid_obo(x, allow, ns_type)
        assertthat::assert_that(all(valid_obo))
    } else {
        # to reliably identify non-OBO IDs
        valid_obo <- is_valid_obo(x, allow = c("standard", "ns.lui"), ns_type)
    }

    if (ns_type == "ont") {
        prefixes <- obo_ont_prefix
    } else if (ns_type == "prop") {
        prefixes <- obo_prop_prefix
    } else {
        prefixes <- obo_prefix[names(obo_prefix) != "obo"]
    }

    out <- reformat_obo_id(x, valid_obo, as, prefixes)
    out
}

#' @examples
#' # format_doid() works the same
#' x <- c(
#'     "http://purl.obolibrary.org/obo/DOID_0001816",
#'     "DOID:4",
#'     "obo:DOID_14566",
#'     "<http://purl.obolibrary.org/obo/DOID_156>"
#' )
#'
#' format_doid(x, as = "curie")
#'
#' # ...but other OBO Foundry ontology IDs will error on validation
#' try(format_doid(c(x, "obo:CL_0000066")))
#'
#' # though they can be passed through if validation is turned off
#' mixed_input <- c(x, "obo:SYMP_0000000", "foaf:Person", "random_text", "0050117")
#' format_doid(mixed_input, validate = FALSE)
#'
#' @rdname format_obo
#' @export
format_doid <- function(x, as = "curie", validate = TRUE,
                       allow = "standard", ns_type = "obo") {
    as <- match.arg(
        as,
        choices = c("curie", "obo_curie", "uri", "<uri>", "ns.lui")
    )

    if (validate) {
        valid_doid <- is_valid_doid(x, allow, ns_type)
        assertthat::assert_that(all(valid_doid))
    } else {
        # to reliably identify non-OBO IDs
        valid_doid <- is_valid_doid(x, allow = c("standard", "ns.lui"), ns_type)
    }

    if (ns_type == "ont") {
        prefixes <- obo_ont_prefix["DOID"]
    } else if (ns_type == "prop") {
        prefixes <- obo_prop_prefix["doid"]
    } else {
        prefixes <- obo_prefix[c("DOID", "doid")]
    }

    out <- reformat_obo_id(x, valid_doid, as, prefixes)
    out
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
#' @seealso [extract_subtree()]
#' @export
format_subtree <- function(subtree_df, top_node) {
    rlang::check_installed("tidygraph", reason = "to use `format_subtree()`")
    top_class <- format_doid(top_node, as = "curie")
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
#' @param as What format to use for the hyperlink, as a string; one of "gs"
#' (Google Sheet), "xlsx" (Excel), or "html".
#' @param ... _(Only for `as = "html"`)_ One or more name-value pairs of html
#' `<a>` [attributes](https://www.w3schools.com/tags/tag_a.asp).
#' @param text _(Optional)_ The text to display for each link, as a character
#' vector. If `NULL` (default), the URL itself will serve as the text. If only
#' one value is provided, it will be recycled.
#' @param preserve The value to return when `url` is `NA`, as a string. One of
#' "url" (default) or "text".
#'
#' @section Excel Note:
#' Use the `openxlsx` pkg to write data with hyperlinks to Excel.
#'
#' @examples
#' format_hyperlink("https://www.google.com", "gs")
#' format_hyperlink("https://www.google.com", "xlsx")
#' format_hyperlink("https://www.google.com", "html")
#'
#' # with 'text' (argument must be named)
#' format_hyperlink("https://www.google.com", "gs", text = "Google")
#' format_hyperlink("https://www.google.com", "xlsx", text = "Google")
#' format_hyperlink("https://www.google.com", "html", text = "Google")
#'
#' # html with <a> attributes
#' format_hyperlink(
#'     "https://www.google.com",
#'     "html",
#'     text = "Google",
#'     target = "_blank",
#'     rel = "external"
#' )
#'
#' # NA values in 'url' are passed through without modification by default. If
#' # 'text' is provided and preferred when 'url' is NA use preserve = "text".
#' format_hyperlink(c("https://www.google.com", NA), "gs")
#' format_hyperlink(
#'     c("https://www.google.com", NA),
#'     "gs",
#'     text = c("Google", "placeholder")
#' )
#'
#' # 'url' is always preserved when 'text' is NA but 'url' is not
#' format_hyperlink(
#'     c("https://www.google.com/", "https://madeup.url.com/fakeID"),
#'     "html",
#'     text = c("google", NA),
#'     preserve = "text"
#' )
#'
#' @export
format_hyperlink <- function(url, as, ..., text = NULL, preserve = "url") {
    as <- match.arg(as, c("gs", "xlsx", "html"))
    preserve <- match.arg(preserve, c("url", "text"))
    if (is.null(text) && preserve == "text") {
        rlang::abort('`preserve` can not be set to "text" when `text` is NULL.')
    }
    if (!is.null(text) && length(text) != 1 && length(text) != length(url)) {
        rlang::abort(
            "`text` must be a string or character vector of the same length as `url`"
        )
    }

    if (as == "gs") {
        warn_attr(...)
        if (is.null(text)) {
            formula <- as.character(glue::glue('=HYPERLINK("{url}")'))
        } else {
            formula <- dplyr::if_else(
                !is.na(text),
                as.character(glue::glue('=HYPERLINK("{url}", "{text}")')),
                as.character(glue::glue('=HYPERLINK("{url}")'))
            )
        }
        formatted <- googlesheets4::gs4_formula(formula)
    }

    if (as == "xlsx") {
        warn_attr(...)
        if (is.null(text)) {
            formatted <- url
            class(formatted) <- "hyperlink"
        } else {
            formatted <- dplyr::if_else(
                !is.na(text),
                as.character(glue::glue('=HYPERLINK("{url}", "{text}")')),
                as.character(glue::glue('=HYPERLINK("{url}")'))
            )
            class(formatted) <- "formula"
        }
    }

    if (as == "html") {
        attr <- list(...)
        if (length(attr) > 0) {
            unnamed <- names(attr) == ""
            if (any(unnamed)) {
                rlang::abort(
                    msg_dots(
                        "All hyperlink attributes in `...` must be named.",
                        ...,
                        .which = unnamed
                    )
                )
            }

            attr <- list(...)
            html_attr <- paste0(
                " ", names(attr), "=", sandwich_text(attr, '"'),
                collapse = ""
            )
        } else {
            html_attr <- NULL
        }

        if (is.null(text)) {
            text <- url
        } else {
            text <- dplyr::if_else(!is.na(text), text, url)
        }

        formatted <- glue::glue(
            '<a href="{url}"{html_attr}>{text}</a>',
            .null = ""
        )
        formatted <- as.character(formatted)
    }

    if (preserve == "url") {
        formatted[is.na(url)] <- NA
    } else {
        if (length(text) == 1) {
            formatted[is.na(url)] <- text
        } else {
            formatted[is.na(url)] <- text[is.na(url)]
        }
    }

    formatted
}


# format_obo()/format_doid() helpers --------------------------------------

reformat_obo_id <- function(x, valid, as, prefixes) {
    prefixes <- length_sort(prefixes, by_name = TRUE, decreasing = TRUE)
    prefix_regex <- paste0(names(prefixes), collapse = "|")
    ns <- stringr::str_match(x, paste0("(?:^|.*[/:])(", prefix_regex, ")"))[, 2]
    lui <- purrr::map2_chr(
        x,
        ns,
        ~ stringr::str_remove_all(.x, paste0(".*", .y, "[:_#]|>?$"))
    )
    # set up namespace & separator
    if (as == "curie") {
        first <- paste0(ns, ":")
    } else {
        # all other formats are modified URIs
        first <- prefixes[ns]
    }

    if (as %in% c("obo_curie", "ns.lui")) {
        first <- stringr::str_remove(first, ".*/")
    }
    if (as == "obo_curie") {
        # with special handling for oboInOwl IDs (do NOT have standard OBO PURLs)
        first <- dplyr::if_else(
            ns == "oboInOwl",
            "oboInOwl:",
            paste0("obo:", first)
        )
    }

    # create full output
    out <- paste0(first, lui)

    # restore non-OBO IDs and add/remove angle brackets
    out[!valid] <- x[!valid]
    if (as == "uri") {
        out <- stringr::str_remove_all(out, "^<|>$")
    } else if (as == "<uri>") {
        out <- stringr::str_replace_all(out, c("^<?" = "<", ">?$" = ">"))
    }

    out
}
