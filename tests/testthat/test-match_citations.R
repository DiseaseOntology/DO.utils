
# type_pub_id() tests -----------------------------------------------------

# data
pmid <- c("33244381", "32308857", "30815196", "30815087", "30815154")
pmcid <- c("PMC6371329", "PMC6371303", "PMC6371266", "PMC7153069",
                "PMC7682973")
doi <- c("10.1093/nar/gki050", "10.1093/nar/gkm829", "10.1038/nbt1346",
              "10.1093/nar/gkm1021", "10.1186/1471-2105-8-S9-S1")
mix <- c(pmid[1], pmcid[1], doi[1])
none <- c(
    "A", # random letter
    # DOI must start with 10, have prefix / suffix (missing an element)
    "10/y", "10x/",
    # missing 'P' of 'PMC',
    "MC7682973"
)
pmid_na <- c("33244381", NA_character_, "30815196", NA_character_, "30815154")

# tests
test_that("types identifiable", {
    expect_equal(type_pub_id(pmid), "pmid")
    expect_equal(type_pub_id(pmcid), "pmcid")
    expect_equal(type_pub_id(doi), "doi")
    expect_equal(type_pub_id(pmid_na), "pmid")
})

test_that("non/mixed types fail", {
    expect_error(type_pub_id(mix), NULL)
    expect_error(type_pub_id(none), NULL)
})
