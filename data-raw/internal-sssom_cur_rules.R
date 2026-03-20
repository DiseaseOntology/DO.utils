## code to prepare `sssom_cur_rules` dataset
rlang::check_installed(c("dplyr", "googlesheets4", "readr"))

# DO-specifc SSSOM-curation rules
sssom_cur_rules <- googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1kNCdz79qLlcspjXDz0qaxeqw9UGRxLzr8x0I8VaDJjc/",
  sheet = "curation_rules",
  col_types = "c"
) |>
  # tidy rule group info
  tidyr::fill("group", .direction = "down") |>
  # keep only complete rules (drops group descriptions)
  dplyr::filter(!is.na(.data$rule_name), !is.na(.data$text))

readr::write_csv(sssom_cur_rules, "data-raw/sssom_cur_rules.csv")

.sssom_cur_rules <- stats::na.omit(sssom_cur_rules$rule_name)
attributes(.sssom_cur_rules) <- NULL

# to migrate older datasets - !!!suggest removal when no longer needed!!!
.sssom_cur_rules_recode <- dplyr::filter(
  sssom_cur_rules,
  !is.na(.data$prior_value)
) |>
  (function(df) setNames(df$rule_name, df$prior_value))()

use_data_internal(.sssom_cur_rules, .sssom_cur_rules_recode, overwrite = TRUE)
