#' Deprecated Website Functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [update_website_use_cases()] instead of `make_use_case_html()` and
#' `make_user_list_html()`.
#' @export
#' @keywords internal
#' @name deprecated-website
make_use_case_html <- function(out_dir = "graphics/website", group = "all") {
  # Used until April 2026 when the use cases were combined into a single
  # searchable `DataTables`-based table.
  lifecycle::deprecate_stop(
    when = "0.3.5",
    what = "make_use_case_html()",
    with = "update_website_use_cases()",
    details = "The Use Cases page has changed, use `update_website_use_cases()`."
  )
}


#' @export
#' @keywords internal
#' @name deprecated-website
make_user_list_html <- function(file) {
  # Used until mid-2022 when the use cases were moved from the 'Collaborators'
  # page to the new 'Use Cases' page and split into 3 sections.
  lifecycle::deprecate_stop(
    when = "0.2.5",
    what = "make_user_list_html()",
    with = "update_website_use_cases()",
    details = "This info has moved from the Collaborators page to the Use Cases page, use `update_website_use_cases()`."
  )
}
