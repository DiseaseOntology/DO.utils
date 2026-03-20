## code to prepare `curation_opts` internal dataset
rlang::check_installed("googlesheets4")
devtools::load_all()

curation_opts <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/edit",
  sheet = "template_options",
  col_types = "c"
) |>
  dplyr::filter(!is.na(.data$template) & !.data$inclusion == "deprecated")


# SPARQL set identifying curation data types ------------------------------

.sparql_dt_motif <- curation_opts |>
    dplyr::filter(!is.na(.data$sparql_dt_motif)) |>
    with(
        purrr::set_names(data_type, sparql_dt_motif)
    ) |>
    length_sort(by_name = TRUE, decreasing = TRUE)

readr::write_csv(curation_opts, "data-raw/curation_opts.csv")

.curation_opts <- dplyr::select(
    curation_opts,
    tidyselect::all_of(c("data_type", "template", "inclusion"))
)

use_data_internal(.sparql_dt_motif, .curation_opts, overwrite = TRUE)
