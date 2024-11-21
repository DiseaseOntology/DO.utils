## code to prepare `curation_opts` internal dataset
rlang::check_installed("googlesheets4")
devtools::load_all()

curation_opts <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/edit",
  sheet = "template_options",
  col_types = "c"
) |>
  dplyr::filter(!is.na(.data$template))

readr::write_csv(curation_opts, "data-raw/curation_opts.csv")

.curation_opts <- dplyr::select(
  curation_opts,
  tidyselect::all_of(c("header", "template", "type"))
)

use_data_internal(.curation_opts, overwrite = TRUE)