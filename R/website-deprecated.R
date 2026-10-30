#' Deprecated Website Functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Replacement:
#' - `make_use_case_html()` --> `update_website_use_cases()`
#' - `make_user_list_html()` --> `update_website_use_cases()`
#' - `make_contributor_html()` --> `update_website_use_cases()`
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
    details = "The Use Cases page has changed, use `update_website_use_cases()`." # nolint: line_length_linter.
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
    details = "This info has moved from the Collaborators page to the Use Cases page, use `update_website_use_cases()`." # nolint: line_length_linter.
  )
}


#' @export
#' @keywords internal
#' @name deprecated-website
make_contributor_html <- function(contrib_df) {
  # Used until May 2026 when contributors were reformatted into a DataTables
  # javascript table, supported by the new GHContrib data collection worfklow.
  lifecycle::deprecate_stop(
    when = "0.3.7",
    what = "make_contributor_html()",
    with = "update_website_contributors()",
    details = c(
      "Output has been reformatted into a DataTables javascript table, use `update_website_contributors()`.", # nolint: line_length_linter.
      "Consider using GHContrib for data collection and management (https://github.com/DiseaseOntology/GHContrib)." # nolint: line_length_linter.
    )
  )
}
