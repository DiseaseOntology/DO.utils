# Internal functions for HTML tables -------------------------------------

# Add table indentation
add_table_indent <- function(html, indent) {
    increment_key <- c(
        "[ \t]*(<[ /]*table)" = 0,
        "[ \t]*(<[ /]*t(head|body|foot))" = 1,
        "[ \t]*(<[ /]*tr)" = 2,
        "[ \t]*(< *t[hd][^e])" = 3
    )
    indent_min <- paste0(rep(indent$type, indent$min), collapse = "")
    indent_increment <- paste0(rep(indent$type, indent$increment), collapse = "")
    indent_key <- purrr::map_chr(
        increment_key,
        ~ paste0(
            indent_min,
            paste0(rep(indent_increment, .x), collapse = ""),
            "\\1"
        )
    )

    stringr::str_replace_all(html, indent_key)
}

#' Copy <thead> to <tfoot> for a single <table> in HTML
#' @noRd
copy_thead_to_tfoot <- function(table_html) {
  tfoot <- stringr::str_extract(
    table_html,
    stringr::regex("[ \t]*<thead>.*</thead>", dotall = TRUE)
  ) |>
    stringr::str_replace_all("thead", "tfoot")

  tfoot_exists <- stringr::str_detect(table_html, "<tfoot>")
  if (!tfoot_exists) {
    out <- stringr::str_replace(
      table_html,
      "([ \t]*</table>)",
      paste0(tfoot, "\n\\1")
    )
  } else {
    exist_tfoot_raw <- stringr::str_extract(
      table_html,
      stringr::regex("[ \t]*<tfoot>.*</tfoot>", dotall = TRUE)
    )
    exist_tfoot <- exist_tfoot_raw |>
      stringr::str_split("\n") |>
      unlist()
    min_tab <- min(stringr::str_count(exist_tfoot, "\t"))
    tfoot_show <- stringr::str_remove(
      exist_tfoot,
      paste0("^\t{0,", min_tab, "}|[[:space:]]+$")
    ) |>
      paste0(collapse = "\n")
    cli::cli_alert_info(
      c(
        "<tfoot> already exists in the HTML:",
        i = tfoot_show
      )
    )
    overwrite <- readLines(
      "Would you like to overwrite the existing <tfoot>? [y/n]:  "
    )
    while (!overwrite %in% c("y", "n")) {
      overwrite <- readLines(
        "Please enter 'y' or 'n':  "
      )
    }
    if (overwrite == "n") {
      cli::cli_alert_info("Keeping existing <tfoot>")
    } else {
      out <- stringr::str_replace(
        table_html,
        stringr::regex("[ \t]*<tfoot>.*</tfoot>", dotall = TRUE),
        tfoot
      )
    }
  }

  out
}
