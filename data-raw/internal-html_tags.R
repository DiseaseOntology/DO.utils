rlang::check_installed(
    c("dplyr", "janitor", "purrr", "rvest", "stringr", "tidyr")
)

raw_element_index <- rvest::read_html("https://www.w3.org/TR/html401/index/elements.html")

index_legend <- raw_element_index |>
    rvest::html_text() |>
    stringr::str_match(
        stringr::regex("legend:(.*)name[^[:alnum:]]", dotall = TRUE, ignore_case = TRUE)
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

use_data_internal(.html_tags, overwrite = TRUE)
