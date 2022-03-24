# format_doid() -----------------------------------------------------------

# input & expected output
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

test_that("format_doid() allow_bare switch works", {
    w_bare <- c(x, "0001816")
    bare_expected <- c(curie, "DOID:0001816")

    expect_error(format_doid(w_bare))
    expect_identical(format_doid(w_bare, allow_bare = TRUE), bare_expected)
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
