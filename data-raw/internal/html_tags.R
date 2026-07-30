## code to prepare `.html_tags` internal dataset ##
#
# HTML tag information is retrieved from the W3C HTML 4.01 specification and
# serves as a reference for parsing and validation of HTML elements in DO
# website curation
#
# NOTE: HTML 4.01 is used as a reference for tag information, but the web
# now supports the HTML Living Standard (https://html.spec.whatwg.org/), which
# includes additional tags and attributes.

rlang::check_installed(
  c("dplyr", "here", "janitor", "purrr", "rvest", "stringr", "tidyr", "vroom")
)

outdir <- here::here("data-raw", "internal")

raw_element_index <- rvest::read_html(
  "https://www.w3.org/TR/html401/index/elements.html"
)

index_legend <- raw_element_index |>
  rvest::html_text() |>
  stringr::str_match(
    stringr::regex(
      "legend:(.*)name[^[:alnum:]]",
      dotall = TRUE,
      ignore_case = TRUE
    )
  ) |>
  (\(x) x[, 2])() |>
  stringr::str_split(",[[:space:]]*") |>
  unlist() |>
  stringr::str_remove(" DTD") |>
  stringr::str_squish() |>
  stringr::str_to_lower() |>
  (\(x) purrr::set_names(x, nm = stringr::str_sub(x, end = 1L)))()

.html_tags <- raw_element_index |>
  rvest::html_table() |>
  (\(x) x[[1]])() |>
  dplyr::rename("deprecated" = "Depr.") |>
  janitor::clean_names() |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, "")),
    deprecated = !is.na(.data$deprecated),
    dplyr::across(
      dplyr::where(is.character),
      ~ .x |>
        stringr::str_to_lower() |>
        dplyr::recode(!!!index_legend)
    )
  ) |>
  tidyr::replace_na(list(start_tag = "required", end_tag = "required")) |>
  dplyr::select(-"empty")

if (nrow(.html_tags) != dplyr::n_distinct(.html_tags$name)) {
  rlang::abort("Duplicate HTML tag names found")
}

# save tabular data for reference
vroom::vroom_write(
  .html_tags,
  file = file.path(outdir, "html4_tags.tsv"),
  na = ""
)

saveRDS(
  .html_tags,
  file = file.path(outdir, "html_tags.rds"),
  compress = "bzip2"
)
