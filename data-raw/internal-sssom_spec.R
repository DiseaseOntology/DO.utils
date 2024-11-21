# Capture official SSSOM specification and parse for use by DO.utils
rlang::check_installed("yaml")
devtools::load_all()

sssom_version <- stringr::str_remove(
    httr::HEAD("https://github.com/mapping-commons/sssom/releases/latest/")$url,
    ".*/"
)
sssom_yaml_path <- glueV(
    "https://raw.githubusercontent.com/mapping-commons/sssom/!<<sssom_version>>!/src/sssom_schema/schema/sssom_schema.yaml"
)
.sssom_spec <- yaml::read_yaml(sssom_yaml_path)
.sssom_spec$version <- sssom_version
.sssom_spec$access_date <- Sys.Date()

.sssom_slot_types <- purrr::map_chr(.sssom_spec$slots, ~ .$range)
.sssom_mapping_slots <- .sssom_spec$classes$mapping$slots

use_data_internal(
    .sssom_spec,
    .sssom_slot_types,
    .sssom_mapping_slots,
    overwrite = TRUE
)
