## code to prepare `.sssom_spec`, `.sssom_slot_types`, and
## `.sssom_mapping_slots` internal datasets ##
#
# Capture official SSSOM specification and parse for use by DO.utils

rlang::check_installed(
  c("glue", "here", "purrr", "stringr", "yaml")
)


# identify latest SSSOM version and construct URL for raw YAML
sssom_version <- stringr::str_remove(
  httr::HEAD("https://github.com/mapping-commons/sssom/releases/latest/")$url,
  ".*/"
)
sssom_yaml_path <- glue::glue(
  "https://raw.githubusercontent.com/mapping-commons/sssom/@sssom_version@/src/sssom_schema/schema/sssom_schema.yaml",
  .open = "@",
  .close = "@"
)


# download YAML schema and parse for internal use
outdir <- here::here("data-raw", "internal")
yaml_file <- file.path(outdir, paste0("sssom_schema-", sssom_version, ".yaml"))

dl_status <- download.file(sssom_yaml_path, yaml_file)

if (dl_status != 0) {
  rlang::abort(
    glue::glue(
      "Failed to download SSSOM specification from {sssom_yaml_path}"
    )
  )
}

.sssom_spec <- yaml::read_yaml(yaml_file)
.sssom_spec$version <- sssom_version
.sssom_spec$access_date <- Sys.Date()

.sssom_slot_types <- purrr::map_chr(.sssom_spec$slots, ~ .$range)
.sssom_mapping_slots <- .sssom_spec$classes$mapping$slots

saveRDS(
  .sssom_spec,
  file = file.path(outdir, "sssom_spec.rds"),
  compress = "bzip2"
)
saveRDS(
  .sssom_slot_types,
  file = file.path(outdir, "sssom_slot_types.rds"),
  compress = "bzip2"
)
saveRDS(
  .sssom_mapping_slots,
  file = file.path(outdir, "sssom_mapping_slots.rds"),
  compress = "bzip2"
)
