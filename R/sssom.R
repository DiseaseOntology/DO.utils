#' Update DO SSSOM Curation Rule Names
#'
#' Recode old DO SSSOM curation rule names to new rule names in both direct
#' rule name columns and comment columns. **THIS FUNCTION WILL BE REMOVED WHEN
#' ONCE ALL CURATION DATASETS ARE UPDATED TO USE THE NEW RULE NAMES.**
#'
#' @param .data A data frame containing SSSOM curation data.
#' @param rule_cols (Optional) Character vector of column name(s) containing
#'   singular curation rule values to be recoded.
#' @param comment_cols (Optional) Character vector of column name(s) containing
#'   comments with "Rules: ..." patterns where rule names should be recoded.
#'
#' @return The data frame `.data` with recoded curation rule names.
#'
#' @examples
#' df <- tibble::tibble(
#'    curation_rule = c("not_disease", "hierarchy (sc)"),
#'    comment = c("Rules: not_disease.", "Rules: hierarchy (sc), other_map.")
#' )
#'
#' # Recode both rule and comment columns
#' recoded_df <- recode_sssom_rules(
#'   df,
#'   rule_cols = c("curation_rule", "other_rule_col"),
#'   comment_cols = c("comment", "notes")
#' )
#'
#' @export
recode_sssom_rules <- function(.data, rule_cols = NULL, comment_cols = NULL) {
  # Check that at least one column type is specified
  if ((length(rule_cols) < 1) && (length(comment_cols) < 1)) {
    rlang::abort(
      "At least one of `rule_cols` or `comment_cols` must be specified"
    )
  }

  out <- .data

  # Recode rule columns (simple direct recoding)
  if (length(rule_cols) > 0) {
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(rule_cols),
          ~ dplyr::recode(.x, !!!.sssom_cur_rules_recode)
        )
      )
  }

  # Recode comment columns (within "Rules: ... ." patterns)
  if (length(comment_cols) > 0) {
    out <- out |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(comment_cols),
          recode_comment_rules
        )
      )
  }

  out
}


# Helper function to recode rule names within "Rules: ..." patterns in comments
recode_comment_rules <- function(x) {
  replace_vctr <- c(
    # standardize "Rules: " prefix and trailing punctuation
    "^[Rr][Uu][Ll][Ee][Ss]:\\s+" = "Rules: ",
    "([^;.[:space:]])\\s*[.;]?$" = "\\1.",
    # recode must be escaped due to () for regex matching
    setNames(
      .sssom_cur_rules_recode,
      stringr::str_escape(names(.sssom_cur_rules_recode))
    )
  )
  out <- stringr::str_replace(
    x,
    "[Rr][Uu][Ll][Ee][Ss]:\\s+.+?\\s*([.;]|$)",
    ~ stringr::str_squish(.x) |>
      stringr::str_replace_all(replace_vctr)
  )
  out
}
