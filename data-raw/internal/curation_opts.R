## code to prepare `.curation_opts` internal dataset ##
#
# This dataset is updated from a a Google Sheet and serves as a schema for
# establishment of curation templates and their conversion to robot templates

rlang::check_installed(
    c("dplyr", "googlesheets4", "here", "vroom")
)


out_dir <- here::here("data-raw", "internal")

# save full schema for developer reference
curation_opts <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1Zn6p5xkVHUwbWe1N8FUa3fNcEkAOoE9P4ADb12f69hQ/edit",
    sheet = "template_options",
    col_types = "c"
) |>
    dplyr::filter(!is.na(.data$template))

vroom::vroom_write(
    curation_opts,
    file.path(out_dir, "curation_opts.tsv"),
    na = ""
)

# save internal data
.curation_opts <- dplyr::select(
    curation_opts,
    dplyr::all_of(c("data_type", "template", "inclusion"))
)

saveRDS(
    .curation_opts,
    file = file.path(out_dir, "curation_opts.rds"),
    compress = "bzip2"
)
