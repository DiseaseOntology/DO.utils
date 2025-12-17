## code to prepare `sssom_cur_rules` dataset goes here
rlang::check_installed(c("dplyr", "googlesheets4", "readr"))

# SSSOM-curation rules (currently specific to DO)
sssom_cur_rules <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1qAzDm9_jFe_a0gqxDpWI9ik8FeSdI-aYr7smbGnSNpk/edit",
    sheet = "curation_rules",
    col_types = "c"
)

readr::write_csv(sssom_cur_rules, "data-raw/sssom_cur_rules.csv")

.sssom_cur_rules <- dplyr::select(sssom_cur_rules, "rule_name") |>
    unlist(use.names = FALSE) |>
    stats::na.omit()
attributes(.sssom_cur_rules) <- NULL

use_data_internal(.sssom_cur_rules, overwrite = TRUE)
