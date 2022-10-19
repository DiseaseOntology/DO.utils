# code to prepare 'pmc' test data

data_dir <- testthat::test_path("data", "pmc")
if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
}


# Test Data ---------------------------------------------------------------

# search
search_pmc_doid <- search_pmc("doid", pmid = TRUE, retmax = 5)


# Save --------------------------------------------------------------------

objs <- c("search_pmc_doid")

data_paths <- file.path(data_dir, paste0(objs, ".rda"))

mapply(save, list = objs, file = data_paths,
       MoreArgs = list(compress = "bzip2", version = "2")
)



