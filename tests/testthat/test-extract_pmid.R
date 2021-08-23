# prep data
pm_res <- readRDS("data/pm_res.rds")
pmc_res_full <- readRDS("data/pmc_res_full.rds")

pmc_res <- pmc_res_full
pmc_res$pmids <- NULL

pmc_res_incomplete <- pmc_res_full
pmc_res_incomplete$pmids[3] <- NA

# tests
test_that("extract_pmid works", {
    expect_identical(
        extract_pmid(pm_res),
        c("30441083", "27638616", "27577487", "25841438", "22080554")
    )
    expect_identical(
        extract_pmid(pmc_res_full),
        c("34197603", "34394377", "34394068", "33625251", "34306572")
    )
})

test_that("extract_pmid catches problems", {
    expect_error(extract_pmid(pmc_res), NULL)
    expect_warning(extract_pmid(pmc_res_incomplete), NULL)
})

