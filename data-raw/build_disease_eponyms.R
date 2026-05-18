# Build the disease_eponyms dataset from disease_eponyms_curated.tsv
#
# Reads the shared curated TSV (populated by omim_eponyms.R and future source
# scripts) and saves the disease_eponyms package dataset.
#
# Workflow:
#   1. Run source-specific scripts (e.g. data-raw/omim_eponyms.R) to mine
#      candidates and add them to data-raw/disease_eponyms_curated.tsv.
#   2. Review "pending" rows and set status to "cap" or "lower".
#   3. source("data-raw/build_disease_eponyms.R") to rebuild the dataset.

devtools::load_all()

curated_tsv <- here::here("data-raw", "disease_eponyms_curated.tsv")
if (!file.exists(curated_tsv)) {
  rlang::abort(c(
    paste0("Curated TSV not found: ", curated_tsv),
    i = "Run data-raw/omim_eponyms.R first to generate it."
  ))
}

curated <- readr::read_tsv(
  curated_tsv,
  col_types = readr::cols(.default = "c"),
  na = ""
)

# Exclude contested entries (word_cap = NA) even if accidentally marked cap
accepted <- curated |>
  dplyr::filter(status == "cap", !is.na(word_cap))
disease_eponyms <- setNames(accepted$word_cap, accepted$word_lower)

usethis::use_data(disease_eponyms, overwrite = TRUE)
message(
  "disease_eponyms dataset saved with ",
  length(disease_eponyms),
  " entries."
)
