# format_obo() ------------------------------------------------------------

# valid OBO input
all_obo <- c(
    # primary namespaces
    curie = "DOID:4",
    uri = "http://purl.obolibrary.org/obo/CL_0001816",
    "<uri>" = "<http://purl.obolibrary.org/obo/GENO_0014566>",
    obo_curie = "obo:UBERON_14566",
    ns.lui = "HP_0040001",
    # secondary namespaces
    curie = "doid:DO_rad_slim",
    obo_curie = "obo:so#has_origin",
    uri = "http://www.geneontology.org/formats/oboInOwl#id",
    "<uri>" = "<http://purl.obolibrary.org/obo/mondo#something_here>",
    ns.lui = "foodon#00002403"
)
non_obo <- c(
    curie = "foaf:Person",
    uri = "http://purl.org/dc/elements/1.1/type",
    "<uri>" = "<http://www.w3.org/2004/02/skos/core#exactMatch>",
    ns.lui = "owl#Class"
)

std_obo <- all_obo[!names(all_obo) == "ns.lui"]
all_id <- c(all_obo, non_obo)

# expected output
all_curie <- c(
    # primary namespaces
    curie = "DOID:4",
    uri = "CL:0001816",
    "<uri>" = "GENO:0014566",
    obo_curie = "UBERON:14566",
    ns.lui = "HP:0040001",
    # secondary namespaces
    curie = "doid:DO_rad_slim",
    obo_curie = "so:has_origin",
    uri = "oboInOwl:id",
    "<uri>" = "mondo:something_here",
    ns.lui = "foodon:00002403"
)

all_obo_curie <- c(
    # primary namespaces
    curie = "obo:DOID_4",
    uri = "obo:CL_0001816",
    "<uri>" = "obo:GENO_0014566",
    obo_curie = "obo:UBERON_14566",
    ns.lui = "obo:HP_0040001",
    # secondary namespaces
    curie = "obo:doid#DO_rad_slim",
    obo_curie = "obo:so#has_origin",
    uri = "oboInOwl:id",
    "<uri>" = "obo:mondo#something_here",
    ns.lui = "obo:foodon#00002403"
)

all_uri <- c(
    # primary namespaces
    curie = "http://purl.obolibrary.org/obo/DOID_4",
    uri = "http://purl.obolibrary.org/obo/CL_0001816",
    "<uri>" = "http://purl.obolibrary.org/obo/GENO_0014566",
    obo_curie = "http://purl.obolibrary.org/obo/UBERON_14566",
    ns.lui = "http://purl.obolibrary.org/obo/HP_0040001",
    # secondary namespaces
    curie = "http://purl.obolibrary.org/obo/doid#DO_rad_slim",
    obo_curie = "http://purl.obolibrary.org/obo/so#has_origin",
    uri = "http://www.geneontology.org/formats/oboInOwl#id",
    "<uri>" = "http://purl.obolibrary.org/obo/mondo#something_here",
    ns.lui = "http://purl.obolibrary.org/obo/foodon#00002403"
)

all_ns.lui <- c(
    # primary namespaces
    curie = "DOID_4",
    uri = "CL_0001816",
    "<uri>" = "GENO_0014566",
    obo_curie = "UBERON_14566",
    ns.lui = "HP_0040001",
    # secondary namespaces
    curie = "doid#DO_rad_slim",
    obo_curie = "so#has_origin",
    uri = "oboInOwl#id",
    "<uri>" = "mondo#something_here",
    ns.lui = "foodon#00002403"
)

# tests
test_that("format_obo(), with defaults, works", {
    curie <- unname(all_curie[!names(all_curie) == "ns.lui"])
    obo_curie <- unname(all_obo_curie[!names(all_obo_curie) == "ns.lui"])
    uri <- unname(all_uri[!names(all_uri) == "ns.lui"])
    bracketed_uri <- paste0("<", uri, ">")
    ns.lui <- unname(all_ns.lui[!names(all_ns.lui) == "ns.lui"])

    expect_identical(format_obo(std_obo, as = "curie"), curie)
    expect_identical(format_obo(std_obo, as = "obo_curie"), obo_curie)
    expect_identical(format_obo(std_obo, as = "uri"), uri)
    expect_identical(format_obo(std_obo, as = "<uri>"), bracketed_uri)
    expect_identical(format_obo(std_obo, as = "ns.lui"), ns.lui)
})

test_that("format_obo() validate switch works", {
    expect_error(format_obo(all_obo))
    expect_error(format_obo(all_id))
    expect_identical(
        format_obo(all_id, validate = FALSE),
        unname(c(all_curie, non_obo))
    )
    expect_identical(
        format_obo(all_id, as = "uri", validate = FALSE),
        unname(c(all_uri, stringr::str_remove_all(non_obo, "^<|>$")))
    )
    expect_identical(
        format_obo(all_id, as = "<uri>", validate = FALSE),
        unname(
            c(
                paste0("<", all_uri, ">"),
                stringr::str_replace_all(non_obo, c("^<?" = "<", ">?$" = ">"))
            )
        )
    )
})

# format_doid() ------------------------------------------------------------

# valid DOID input
all_doid <- c(
    # primary namespaces
    curie = "DOID:4",
    uri = "http://purl.obolibrary.org/obo/DOID_0001816",
    "<uri>" = "<http://purl.obolibrary.org/obo/DOID_0014566>",
    obo_curie = "obo:DOID_14566",
    ns.lui = "DOID_0040001",
    # secondary namespaces
    curie = "doid:DO_rad_slim",
    obo_curie = "obo:doid#has_origin",
    uri = "http://purl.obolibrary.org/obo/doid#id",
    "<uri>" = "<http://purl.obolibrary.org/obo/doid#something_here>",
    ns.lui = "doid#00002403"
)
non_doid <- c(
    curie = "foaf:Person",
    uri = "http://www.geneontology.org/formats/oboInOwl#id",
    "<uri>" = "<http://www.w3.org/2004/02/skos/core#exactMatch>",
    ns.lui = "CL_0001816"
)

std_doid <- all_doid[!names(all_doid) == "ns.lui"]
all_id <- c(all_doid, non_doid)

# expected output
all_curie <- c(
    # primary namespaces
    curie = "DOID:4",
    uri = "DOID:0001816",
    "<uri>" = "DOID:0014566",
    obo_curie = "DOID:14566",
    ns.lui = "DOID:0040001",
    # secondary namespaces
    curie = "doid:DO_rad_slim",
    obo_curie = "doid:has_origin",
    uri = "doid:id",
    "<uri>" = "doid:something_here",
    ns.lui = "doid:00002403"
)

all_obo_curie <- c(
    # primary namespaces
    curie = "obo:DOID_4",
    uri = "obo:DOID_0001816",
    "<uri>" = "obo:DOID_0014566",
    obo_curie = "obo:DOID_14566",
    ns.lui = "obo:DOID_0040001",
    # secondary namespaces
    curie = "obo:doid#DO_rad_slim",
    obo_curie = "obo:doid#has_origin",
    uri = "obo:doid#id",
    "<uri>" = "obo:doid#something_here",
    ns.lui = "obo:doid#00002403"
)

all_uri <- c(
    # primary namespaces
    curie = "http://purl.obolibrary.org/obo/DOID_4",
    uri = "http://purl.obolibrary.org/obo/DOID_0001816",
    "<uri>" = "http://purl.obolibrary.org/obo/DOID_0014566",
    obo_curie = "http://purl.obolibrary.org/obo/DOID_14566",
    ns.lui = "http://purl.obolibrary.org/obo/DOID_0040001",
    # secondary namespaces
    curie = "http://purl.obolibrary.org/obo/doid#DO_rad_slim",
    obo_curie = "http://purl.obolibrary.org/obo/doid#has_origin",
    uri = "http://purl.obolibrary.org/obo/doid#id",
    "<uri>" = "http://purl.obolibrary.org/obo/doid#something_here",
    ns.lui = "http://purl.obolibrary.org/obo/doid#00002403"
)

all_ns.lui <- stringr::str_remove(all_obo_curie, "obo:")

# tests
test_that("format_doid(), with defaults, works", {
    curie <- unname(all_curie[!names(all_curie) == "ns.lui"])
    obo_curie <- unname(all_obo_curie[!names(all_obo_curie) == "ns.lui"])
    uri <- unname(all_uri[!names(all_uri) == "ns.lui"])
    bracketed_uri <- paste0("<", uri, ">")
    ns.lui <- unname(all_ns.lui[!names(all_ns.lui) == "ns.lui"])

    expect_identical(format_doid(std_doid, as = "curie"), curie)
    expect_identical(format_doid(std_doid, as = "obo_curie"), obo_curie)
    expect_identical(format_doid(std_doid, as = "uri"), uri)
    expect_identical(format_doid(std_doid, as = "<uri>"), bracketed_uri)
    expect_identical(format_doid(std_doid, as = "ns.lui"), ns.lui)
})

test_that("format_doid() validate switch works", {
    expect_error(format_doid(all_doid))
    expect_error(format_doid(all_id))
    expect_identical(
        format_doid(all_id, validate = FALSE),
        unname(c(all_curie, non_doid))
    )
    expect_identical(
        format_doid(all_id, as = "uri", validate = FALSE),
        unname(c(all_uri, stringr::str_remove_all(non_doid, "^<|>$")))
    )
    expect_identical(
        format_doid(all_id, as = "<uri>", validate = FALSE),
        unname(
            c(
                paste0("<", all_uri, ">"),
                stringr::str_replace_all(non_doid, c("^<?" = "<", ">?$" = ">"))
            )
        )
    )
})


# format_subtree() --------------------------------------------------------

# data
subtree <- readr::read_csv("data/format_subtree.csv", col_types = "cccc")
res <- readr::read_csv(
    "data/format_subtree-res.csv",
    col_types = "ccclcccc"
)

# tests
test_that("format_subtree() works", {
    skip_if_not_installed("tidygraph")
    expect_identical(format_subtree(subtree, "DOID:3070"), res)
})

test_that("format_subtree() works when no fill is needed", {
    skip_if_not_installed("tidygraph")
    # remove high grade ependymoma = no duplication --> no fill needed
    no_hge <- dplyr::filter(
        subtree,
        id != "DOID:5074",
        parent_id != "DOID:5074"
    )

    # make res look as though hge was never there
    hge_pos <- which(res$id == "DOID:5074")
    no_hge_res <- dplyr::filter(
        res,
        dplyr::row_number() < hge_pos |
            dplyr::row_number() > hge_pos + 2
    ) %>%
        dplyr::mutate(
            duplicated = FALSE,
            parent_id = stringr::str_remove(parent_id, "\\|?DOID:5074\\|?"),
            parent_label = stringr::str_remove(
                parent_label, "\\|?high grade ependymoma\\|?"
            )
        )

    expect_identical(format_subtree(no_hge, "DOID:3070"), no_hge_res)
})


# format_axiom() ----------------------------------------------------------

axioms <- readr::read_lines("data/axioms.ofn")
prop_df <- readr::read_csv("data/property_df.csv", col_types = "c")

test_that("format_axiom() works", {
    expect_snapshot(format_axiom(axioms))
    expect_snapshot(format_axiom(axioms, prop_df))
    expect_snapshot(format_axiom(axioms, generify_obo = TRUE))
    expect_snapshot(format_axiom(axioms, prop_df, generify_obo = TRUE))
})



# format_hyperlink() ------------------------------------------------------

test_that("format_hyperlink() works", {
    url <- "https://www.google.com/"

    gs_expect <- '=HYPERLINK(\"https://www.google.com/\")'
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- "https://www.google.com/"
    class(xlsx_expect) <- "hyperlink"

    expect_equal(format_hyperlink(url, "gs"), gs_expect)
    expect_equal(format_hyperlink(url, "xlsx"),xlsx_expect)
    expect_equal(
        format_hyperlink(url, "html"),
        "<a href=\"https://www.google.com/\">https://www.google.com/</a>"
    )
})

test_that("format_hyperlink() text arg works", {
    url <- "https://www.google.com/"
    text <- "google"

    gs_expect <- '=HYPERLINK(\"https://www.google.com/\", \"google\")'
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- '=HYPERLINK(\"https://www.google.com/\", \"google\")'
    class(xlsx_expect) <- "formula"

    expect_equal(format_hyperlink(url, "gs", text = text), gs_expect)
    expect_equal(format_hyperlink(url, "xlsx", text = text), xlsx_expect)
    expect_equal(
        format_hyperlink(url, "html", text = text),
        "<a href=\"https://www.google.com/\">google</a>"
    )
})

test_that("format_hyperlink(as = 'html') works with attributes", {
    expect_equal(
        format_hyperlink(
            "https://www.google.com/",
            "html",
            target = "_blank",
            rel = "external"
        ),
        "<a href=\"https://www.google.com/\" target=\"_blank\" rel=\"external\">https://www.google.com/</a>"
    )

    expect_equal(
        format_hyperlink(
            "https://www.google.com/",
            "html",
            target = "_blank",
            rel = "external",
            text = "google"
        ),
        "<a href=\"https://www.google.com/\" target=\"_blank\" rel=\"external\">google</a>"
    )

    expect_error(
        format_hyperlink(
            "https://www.google.com/",
            "html",
            target = "_blank",
            "external", #unnamed argument
            text = "google"
        ),
        regexp = '"external"'
    )
})

test_that("format_hyperlink() warns when attributes used and not as = 'html'", {
    gs_expect <- '=HYPERLINK(\"https://www.google.com/\")'
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- "https://www.google.com/"
    class(xlsx_expect) <- "hyperlink"

    expect_warning(
        expect_equal(
            format_hyperlink(
                "https://www.google.com/",
                "gs",
                target = "_blank",
                rel = "external"
            ),
            gs_expect
        )
    )
    expect_warning(
        expect_equal(
            format_hyperlink(
                "https://www.google.com/",
                "xlsx",
                target = "_blank",
                rel = "external"
            ),
            xlsx_expect
        )
    )
})

test_that("format_hyperlink(preserve = 'url') returns NA from url input", {
    url <- c("https://www.google.com/", NA)

    gs_expect <- c('=HYPERLINK(\"https://www.google.com/\")', NA)
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- c("https://www.google.com/", NA)
    class(xlsx_expect) <- "hyperlink"

    expect_equal(format_hyperlink(url, "gs"), gs_expect)
    expect_equal(format_hyperlink(url, "xlsx"), xlsx_expect)
    expect_equal(
        format_hyperlink(url, "html"),
        c("<a href=\"https://www.google.com/\">https://www.google.com/</a>", NA)
    )
})

test_that("format_hyperlink(preserve = 'text') errors without text input", {
    url <- "https://www.google.com/"

    expect_error(format_hyperlink(url, "gs", preserve = 'text'))
    expect_error(format_hyperlink(url, "xlsx", preserve = 'text'))
    expect_error(format_hyperlink(url, "html", preserve = 'text'))
})

test_that("format_hyperlink(preserve = 'text') returns text input", {
    url <- c("https://www.google.com/", NA)
    text <- c("google", "blah")

    gs_expect <- c('=HYPERLINK(\"https://www.google.com/\", \"google\")', "blah")
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- c('=HYPERLINK(\"https://www.google.com/\", \"google\")', "blah")
    class(xlsx_expect) <- "formula"

    expect_equal(
        format_hyperlink(url, "gs", text = text, preserve = "text"),
        gs_expect
    )
    expect_equal(
        format_hyperlink(url, "xlsx", text = text, preserve = "text"),
        xlsx_expect
    )
    expect_equal(
        format_hyperlink(url, "html", text = text, preserve = "text"),
        c("<a href=\"https://www.google.com/\">google</a>", "blah")
    )
})

test_that("format_hyperlink(preserve = 'text') returns text NA input", {
    url <- c("https://www.google.com/", NA)
    text <- c("google", NA)

    gs_expect <- c('=HYPERLINK(\"https://www.google.com/\", \"google\")', NA)
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- c('=HYPERLINK(\"https://www.google.com/\", \"google\")', NA)
    class(xlsx_expect) <- "formula"

    expect_equal(
        format_hyperlink(url, "gs", text = text, preserve = "text"),
        gs_expect
    )
    expect_equal(
        format_hyperlink(url, "xlsx", text = text, preserve = "text"),
        xlsx_expect
    )
    expect_equal(
        format_hyperlink(url, "html", text = text, preserve = "text"),
        c("<a href=\"https://www.google.com/\">google</a>", NA)
    )
})

test_that("format_hyperlink(preserve = 'text') returns url when ONLY text is NA", {
    url <- c("https://www.google.com/", "https://madeup.url.com/fakeID")
    text <- c("google", NA)

    gs_expect <- c(
        '=HYPERLINK(\"https://www.google.com/\", \"google\")',
        '=HYPERLINK(\"https://madeup.url.com/fakeID\")'
    )
    class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

    xlsx_expect <- c(
        '=HYPERLINK(\"https://www.google.com/\", \"google\")',
        '=HYPERLINK(\"https://madeup.url.com/fakeID\")'
    )
    class(xlsx_expect) <- "formula"

    expect_equal(
        format_hyperlink(url, "gs", text = text, preserve = "text"),
        gs_expect
    )
    expect_equal(
        format_hyperlink(url, "xlsx", text = text, preserve = "text"),
        xlsx_expect
    )
    expect_equal(
        format_hyperlink(url, "html", text = text, preserve = "text"),
        c(
            "<a href=\"https://www.google.com/\">google</a>",
            "<a href=\"https://madeup.url.com/fakeID\">https://madeup.url.com/fakeID</a>"
        )
    )
})
