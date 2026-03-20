## code to prepare `.sssom_cur_rules` dataset ##
#
# DO-specific curation rules for SSSOM-curation templates.

rlang::check_installed(c("dplyr", "googlesheets4", "here", "vroom"))

outdir <- here::here("data-raw", "internal")

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

# save curation rules with details for reference
vroom::vroom_write(
  sssom_cur_rules,
  file.path(outdir, "sssom_cur_rules.tsv"),
  na = ""
)

# save only rule names for use in validation (internal data)
.sssom_cur_rules <- stats::na.omit(sssom_cur_rules$rule_name)
attributes(.sssom_cur_rules) <- NULL

saveRDS(
  .sssom_cur_rules,
  file = file.path(outdir, "sssom_cur_rules.rds"),
  compress = "bzip2"
)


# save to migrate older datasets - !!!suggest removal when no longer needed!!!
.sssom_cur_rules_recode <- dplyr::filter(
  sssom_cur_rules,
  !is.na(.data$prior_value)
) |>
  (function(df) setNames(df$rule_name, df$prior_value))()


saveRDS(
  .sssom_cur_rules_recode,
  file = file.path(outdir, "sssom_cur_rules_recode.rds"),
  compress = "bzip2"
)
