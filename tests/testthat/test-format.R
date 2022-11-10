# format_doid() -----------------------------------------------------------

# valid DOID input & expected output
x <- c("http://purl.obolibrary.org/obo/DOID_0001816", "DOID:4",
       "obo:DOID_14566", "DOID_0040001")

curie <- c("DOID:0001816", "DOID:4", "DOID:14566", "DOID:0040001")
uri <- c("http://purl.obolibrary.org/obo/DOID_0001816",
         "http://purl.obolibrary.org/obo/DOID_4",
         "http://purl.obolibrary.org/obo/DOID_14566",
         "http://purl.obolibrary.org/obo/DOID_0040001")
obo_curie <- c("obo:DOID_0001816", "obo:DOID_4", "obo:DOID_14566",
               "obo:DOID_0040001")
basename <- c("DOID_0001816", "DOID_4", "DOID_14566", "DOID_0040001")


# tests
test_that("format_doid() works", {
    expect_identical(format_doid(x, as = "URI"), uri)
    expect_identical(format_doid(x, as = "CURIE"), curie)
    expect_identical(format_doid(x, as = "obo_CURIE"), obo_curie)
    expect_identical(format_doid(x, as = "basename"), basename)
})

test_that("format_doid() convert_bare switch works", {
    w_bare <- c(x, "0050117")
    bare_expected <- c(curie, "DOID:0050117")

    expect_error(format_doid(w_bare))
    expect_identical(format_doid(w_bare, convert_bare = TRUE), bare_expected)
})

test_that("format_doid() validate_input switch works", {
    not_valid <- c("blah", "obo:SYMP_0000000")
    mixed <- c(x, not_valid, "0050117")

    expect_identical(
        format_doid(mixed, validate_input = FALSE),
        c(curie, not_valid, "0050117")
    )
    expect_identical(
        format_doid(mixed, convert_bare = TRUE, validate_input = FALSE),
        c(curie, not_valid, "DOID:0050117")
    )
})


# format_obo() ------------------------------------------------------------

# valid OBO input & expected output
x <- c("http://purl.obolibrary.org/obo/DOID_0001816",
       "<http://purl.obolibrary.org/obo/CL_0000066>",
       "obo:so#has_origin",
       "obo:SYMP_0000000")

curie <- c("obo:DOID_0001816", "obo:CL_0000066", "obo:so#has_origin",
           "obo:SYMP_0000000")
uri <- c("http://purl.obolibrary.org/obo/DOID_0001816",
         "http://purl.obolibrary.org/obo/CL_0000066",
         "http://purl.obolibrary.org/obo/so#has_origin",
         "http://purl.obolibrary.org/obo/SYMP_0000000")
bracketed_uri <- c("<http://purl.obolibrary.org/obo/DOID_0001816>",
                   "<http://purl.obolibrary.org/obo/CL_0000066>",
                   "<http://purl.obolibrary.org/obo/so#has_origin>",
                   "<http://purl.obolibrary.org/obo/SYMP_0000000>")
ns_lui <- c("DOID_0001816", "CL_0000066", "so#has_origin", "SYMP_0000000")
ns <- c("DOID", "CL", "so#", "SYMP")

# tests
test_that("format_obo() works", {
    expect_identical(format_obo(x, as = "CURIE"), curie)
    expect_identical(format_obo(x, as = "URI"), uri)
    expect_identical(format_obo(x, as = "bracketed_URI"), bracketed_uri)
    expect_identical(format_obo(x, as = "ns_lui"), ns_lui)
    expect_identical(format_obo(x, as = "ns"), ns)
})

test_that("format_obo() validate_input switch works", {
    not_valid <- c("blah", "obo:SYMP:0000000")
    mixed <- c(x, not_valid, "0050117")

    expect_identical(
        format_obo(mixed, validate_input = FALSE),
        c(curie, not_valid, "0050117")
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
    expect_identical(format_subtree(subtree, "DOID:3070"), res)
})

test_that("format_subtree() works when no fill is needed", {
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

# input
url <- c("https://www.google.com/", "https://disease-ontology.org/")
txt <- c("google", "DO")

# expectations
ws_expect <- c(
    '=HYPERLINK(\"https://www.google.com/\", \"google\")',
    '=HYPERLINK(\"https://disease-ontology.org/\", \"DO\")'
)
gs_expect <- ws_expect
class(gs_expect) <- c("googlesheets4_formula", "vctrs_vctr")

xlsx_expect <- ws_expect
class(xlsx_expect) <- "formula"

html_expect <- c(
    "<a href=\"https://www.google.com/\">google</a>",
    "<a href=\"https://disease-ontology.org/\">DO</a>"
)
html_attr_expect <- c(
    "<a href=\"https://www.google.com/\" target=\"_blank\" rel=\"external\">google</a>",
    "<a href=\"https://disease-ontology.org/\" target=\"_blank\" rel=\"external\">DO</a>"
)


test_that("format_hyperlink() works", {
    expect_equal(format_hyperlink(url, txt, "gs"), gs_expect)
    expect_equal(format_hyperlink(url, txt, "xlsx"), xlsx_expect)
    expect_equal(format_hyperlink(url, txt, "html"), html_expect)
})

test_that("format_hyperlink() ... works for html attributes", {
    expect_equal(
        format_hyperlink(url, txt, "html", target = "_blank", rel = "external"),
        html_attr_expect
    )

    # ignored for other formats
    expect_equal(
        format_hyperlink(url, txt, "gs", target = "_blank", rel = "external"),
        gs_expect
    )
    expect_equal(
        format_hyperlink(url, txt, "xlsx",target = "_blank", rel = "external"),
        xlsx_expect
    )
})

test_that("format_hyperlink() preserve_NA arg works", {
    url <- c(url, NA)
    txt <- c(txt, "blah")

    # default: TRUE
    expect_equal(format_hyperlink(url, txt, "gs"), c(gs_expect, NA))

    xlsx_expect <- c(xlsx_expect, NA)
    class(xlsx_expect) <- "formula"
    expect_equal(format_hyperlink(url, txt, "xlsx"), xlsx_expect)

    expect_equal(format_hyperlink(url, txt, "html"), c(html_expect, NA))

    # FALSE
    gs_expect_na <- c(gs_expect, '=HYPERLINK(\"NA\", \"blah\")')
    class(gs_expect_na) <- c("googlesheets4_formula", "vctrs_vctr")
    expect_equal(
        format_hyperlink(url, txt, "gs", preserve_NA = FALSE),
        gs_expect_na
    )

    xlsx_expect_na <- c(xlsx_expect[1:2], '=HYPERLINK(\"NA\", \"blah\")')
    class(xlsx_expect_na) <- "formula"
    expect_equal(
        format_hyperlink(url, txt, "xlsx", preserve_NA = FALSE),
        xlsx_expect_na
    )

    expect_equal(
        format_hyperlink(url, txt, "html", preserve_NA = FALSE),
        c(html_expect, "<a href=\"NA\">blah</a>")
    )
})
