# is_abbreviation() with helpers ------------------------------------------

is_abbreviation <- function(x) {
    stringr::str_detect(x,"^[A-Z0-9]{1,8}[ +/\\-]?([A-Z0-9]{1,8})?$") &
        !(has_dict_word(x) & has_space(x))
}

# helpers #
has_space <- function(x) {
    stringr::str_detect(x, " ")
}


# has_dict_word() with helpers --------------------------------------------

has_dict_word <- function(x) {
    tokens <- stringr::str_split(x, " ")
    token_df <- tibble::tibble(
        idx = vctrs::vec_rep_each(seq_along(tokens), lengths(tokens)),
        token = purrr::flatten_chr(tokens),
        ignore = ignore(token),
        is_word = hunspell::hunspell_check(token),
        has_word = is_word & !ignore
    )

    token_df %>%
        dplyr::group_by(idx) %>%
        dplyr::summarize(has_word = any(has_word)) %>%
        .$has_word
}

# helpers #
ignore <- function(x) {
    is_letter(x) | has_num(x) | is_roman_numeral(x) | is_short(x)
}

is_letter <- function(x) {
    x %in% c(letters, LETTERS)
}

has_num <- function(x) {
    stringr::str_detect(x, "[0-9]")
}

is_roman_numeral <- function(x, .max = 100) {
    x %in% as.character(as.roman(1:.max))
}

is_short <- function(x, n_short = 4) {
    stringr::str_length(x) <= n_short
}
