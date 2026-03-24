## code to prepare `.DO_gs` internal dataset ##
#
# Serves as a reference for DO-related Google Sheets and relevant sheets (tabs)
# for data retrieval

rlang::check_installed("here")

.DO_gs <- list(
  users = list(
    ss = "https://docs.google.com/spreadsheets/d/1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY/",
    sheet = "DO_website_user_list"
  ),
  citedby = list(
    ss = "https://docs.google.com/spreadsheets/d/1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY/",
    sheet = "cited_by"
  )
)

saveRDS(
  .DO_gs,
  file = here::here("data-raw", "internal", "DO_gs.rds"),
  compress = "bzip2"
)
