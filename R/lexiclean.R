#' Prepare Text for Matching
#'
#' Process text to improve exact, quasi-exact and/or lexical text matching
#' (e.g. "lexical cleaning").
#'
#' @param x A character vector.
#' @param mod Desired modifications, as a character string:
#' - `"all"` : Use all modifications.
#' - `"lc"` : Convert to lowercase.
#' - `"roman"` : Convert all numbers to roman numerals.
#' - `"rm_space"` : Remove all spaces, defined by `[:space:]`, see [regex()].
#' - `"rm_punct"` : Remove all punctuation, defined by `[:punct:]`, see
#'  [regex()].
#'
#' @returns A character vector with specified modifications applied.
#'
#' @examples
#' x <- c("disease", "X-linked exudative vitreoretinopathy 2", "hepatitis D",
#'        "dilated cardiomyopathy 1FF", "chromosome 15q25 deletion syndrome",
#'        "spinocerebellar ataxia type 19/22", "Addison's disease",
#'        "disease type II", "B-lymphoblastic leukemia/lymphoma, BCR-ABL1–like")
#'
#' lexiclean(x)
#' lexiclean(x, "roman")
#'
#' @export
lexiclean <- function(x, mod = "all") {
    mods <- c("lc", "roman", "rm_space", "rm_punct")
    mod <- match.arg(mod, choices = c("all", mods), several.ok = TRUE)
    if ("all" %in% mod) mod <- mods

    out <- x
    if ("roman" %in% mod) out <- chr_num_to_roman(out)
    if ("rm_space" %in% mod) out <- stringr::str_remove_all(out, "[[:space:]]")
    if ("rm_punct" %in% mod) out <- stringr::str_remove_all(out, "[[:punct:]]")
    if ("lc" %in% mod) out <- stringr::str_to_lower(out)
    out
}


# lexiclean() helpers -----------------------------------------------------

chr_num_to_roman <- function(x) {
    has_number <- stringr::str_detect(x, "[0-9]+")
    numbers <- stringr::str_extract_all(x, "[0-9]+")
    rn <- purrr::map(
        numbers,
        ~ as.character(utils::as.roman(.x))
    )
    replace_list <- purrr::map2(
        numbers,
        rn,
        function(num, rn) {
            if (length(num) == 0) {
                NA
            } else {
                pattern <- paste0("(^|[^0-9])", num, "([^0-9]|$)")
                purrr::set_names(rn, num)
            }
        }
    )

    purrr::map2_chr(
        x,
        replace_list,
        function(input, patt_repl) {
            if (all(is.na(patt_repl))) {
                input
            } else {
                stringr::str_replace_all(input, patt_repl)
            }
        }
    )
}
