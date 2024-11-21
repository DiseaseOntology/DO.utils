## code to save DO Google Sheet information internally (`.DO_gs`)
devtools::load_all()

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

use_data_internal(.DO_gs, overwrite = TRUE)
