
convert_pubmed_date <- function(x) {
    months <- stringr::str_extract(x, month_regex)





}

month_regex <- vctr_to_string(month.abb, delim = "|")
