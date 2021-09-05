pm_res <- search_pubmed("doid", retmax = 5)
saveRDS(pm_res, file = "tests/testthat/data/pm_res.rds")

pmc_res_full <- search_pmc("doid", pmid = TRUE, retmax = 5)
saveRDS(pmc_res_full, file = "tests/testthat/data/pmc_res_full.rds")


