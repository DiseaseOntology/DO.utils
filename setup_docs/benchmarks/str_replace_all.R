# This shows how str_replace_all performs with a large number of patterns
# to replace in a large text vector, using replacement of IDs in OWL anonymous
# class relations with labels.
#
# Four variations are included for replacement
# 1) Using a named vector of replacements
# 2) Using a named vector of replacements, filtering to only the replacements
#   needed
# 3) Using a function to lookup replacements from a named vector
# 4) Using a function to lookup replacements from a named vector, filtering to
#   only the replacements needed

library(microbenchmark)
library(tidyverse)
devtools::load_all()

temp_owl <- tempfile(fileext = ".owl")
download.file("http://purl.obolibrary.org/obo/doid/doid-merged.owl", temp_owl)
temp_tsv <- tempfile(fileext = ".tsv")
robot(
    "export",
    i = temp_owl,
    header = '"ID|LABEL|Equivalent Class [ANON ID]|SubClass Of [ANON ID]|Disjoint With [ANON ID]"',
    include = '"classes properties"',
    export = temp_tsv
)
full <- readr::read_tsv(
    temp_tsv,
    col_names = c("id", "label", "eq class anon", "subclass anon", "disjoint class anon"),
    skip = 1,
    show_col_types = FALSE
)
x <- tidyr::pivot_longer(
    full,
    cols = c("eq class anon", "subclass anon", "disjoint class anon"),
    names_to = "data_type",
    values_to = "value",
    values_drop_na = TRUE
)

extract_search <- function(x, full, id = FALSE, approach = "replace") {
    approach <- match.arg(approach, c("replace", "recode"))
    if (id) {
        id_keep <- stringr::str_extract_all(x$value, "[A-Za-z0-9_]+:[A-Za-z0-9_#]+") |>
            unlist() |>
            unique()
        full <- dplyr::filter(full, .data$id %in% id_keep)
    } else {
        full <- dplyr::filter(full, !is.na(.data$label))
    }
    label_df <- full |>
        dplyr::select("id", "label") |>
        unique() |>
        dplyr::mutate(
            label = dplyr::case_when(
                stringr::str_detect(.data$label, "'") ~ sandwich_text(.data$label, '"'),
                stringr::str_detect(.data$label, "[^[:alnum:]#_.+-]") ~ sandwich_text(.data$label, "'"),
                TRUE ~ .data$label
            )
        ) |>
        dplyr::arrange(-stringr::str_length(.data$id), .data$id)

    label_replace <- purrr::set_names(
        label_df$label,
        label_df$id
    )

    if (approach == "replace") {
        out <- dplyr::mutate(
            x,
            value = stringr::str_replace_all(.data$value, label_replace)
        )
    } else {
        out <- dplyr::mutate(
            x,
            value = stringr::str_replace_all(
                .data$value,
                "[A-Za-z0-9_]+:[A-Za-z0-9_#]+",
                function(x) dplyr::coalesce(label_replace[x], x)
            )
        )
    }
    out
}

microbenchmark::microbenchmark(
    a1 <- extract_search(axiom[[1]], axiom[[2]], F),
    a2 <- extract_search(axiom[[1]], axiom[[2]], T),
    a3 <- extract_search(axiom[[1]], axiom[[2]], F, approach = "recode"),
    a4 <- extract_search(axiom[[1]], axiom[[2]], T, approach = "recode"),
    times = 4,
    check = "equivalent"
)
