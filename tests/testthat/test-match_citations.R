
# Data --------------------------------------------------------------------

# vctr - good
pmid <- c("33244381", "32308857", "30815196", "30815087", "30815154")
pmcid <- c("PMC6371329", "PMC6371303", "PMC6371266", "PMC7153069",
           "PMC7682973")
doi <- c("10.1093/nar/gki050", "10.1093/nar/gkm829", "10.1038/nbt1346",
         "10.1093/nar/gkm1021", "10.1186/1471-2105-8-S9-S1")

# vctr - bad
mix <- c(pmid[1], pmcid[1], doi[1])
none <- c(
    "A", # random letter
    # DOI must start with 10, have prefix / suffix (missing an element)
    "10/y", "10x/",
    # missing 'P' of 'PMC',
    "MC7682973"
)

# NA
pmid_na <- c("33244381", "32308857", NA, NA, NA)
pmcid_na <- c("PMC6371329", "PMC6371303", "PMC6371266", NA, NA)
doi_na <- c("10.1093/nar/gki050", "10.1093/nar/gkm829", "10.1038/nbt1346",
         "10.1093/nar/gkm1021", NA)

# df - good
df_pmid <- data.frame(n = 1:5, pmid = pmid)
df_all <- dplyr::mutate(df_pmid, pmcid = pmcid, doi = doi)
df_no_pmid <- dplyr::select(df_all, -pmid)
df_name_type_mismatch <- dplyr::mutate(df_pmid, pmid = pmcid)
df_all_na <- data.frame(n = 1:5, pmid = pmid_na, pmcid = pmcid_na, doi = doi_na)

# df - bad
df_wrong_name <- dplyr::rename(df_pmid, bad_name = pmid)


# match_citations() tests -------------------------------------------------

test_that("exact matches work", {
    expect_identical(match_citations(pmid, pmid), 1:5)
    expect_identical(match_citations(pmid, pmid), 1:5)
    expect_message(
        expect_identical(match_citations(df_all, df_all), 1:5)
    )
})

test_that("partial matches work", {
    expect_identical(match_citations(pmid, pmid_na), c(1:2, rep(NA, 3)))
    expect_message(
        expect_identical(match_citations(df_all, df_all_na), c(1:4, NA))
    )
})


test_that("single vctr-df matches work", {
    expect_identical(match_citations(df_pmid, pmid), 1:5)
    expect_identical(match_citations(pmid, df_pmid), 1:5)
})

test_that("mismatches fail", {
    expect_error(match_citations(pmid, pmcid), NULL)
})


# find_pub_id_cols() tests ------------------------------------------------

test_that("find cols works", {
    expect_equal(find_pub_id_cols(df_pmid), "pmid")
    expect_equal(find_pub_id_cols(df_all), c("pmid", "pmcid", "doi"))
})

test_that("missing col fails", {
    expect_error(find_pub_id_cols(df_wrong_name), NULL)
})


# get_pub_id_col() tests --------------------------------------------------

test_that("get cols works", {
    expect_identical(get_pub_id_col(df_pmid, "pmid"), pmid)
    expect_identical(get_pub_id_col(df_all, "pmcid"), pmcid)
    expect_identical(get_pub_id_col(df_all, "doi"), doi)
})

test_that("df name-type mismatch fails", {
    expect_error(get_pub_id_col(df_name_type_mismatch, "pmid"), NULL)
})


# type_pub_id() tests -----------------------------------------------------

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
