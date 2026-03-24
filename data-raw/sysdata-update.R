rlang::check_installed("here", "usethis")

indir <- here::here("data-raw", "internal")


# DO Google Sheets reference
.DO_gs <- readRDS(file.path(indir, "DO_gs.rds"))


# HTML tags reference
.html_tags <- readRDS(file.path(indir, "html_tags.rds"))


# curation template specification
.curation_opts <- readRDS(file.path(indir, "curation_opts.rds"))


# SSSOM specification
.sssom_spec <- readRDS(file.path(indir, "sssom_spec.rds"))
.sssom_slot_types <- readRDS(file.path(indir, "sssom_slot_types.rds"))
.sssom_mapping_slots <- readRDS(file.path(indir, "sssom_mapping_slots.rds"))


usethis::use_data(
  .DO_gs,
  .html_tags,
  .curation_opts,
  .sssom_spec,
  .sssom_slot_types,
  .sssom_mapping_slots,
  internal = TRUE,
  overwrite = TRUE
)
