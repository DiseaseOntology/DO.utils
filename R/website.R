#' Update Table on Use Cases Page
#'
#' Updates the data in the table on the "Use Cases" page of disease-ontology.org
#' to match the curated use cases in the
#' [DO_uses](https://docs.google.com/spreadsheets/d/1wG-d0wt-9YbwhQTaelxqRzbm4qnu11WDM2rv3THy5mY/?gid=1972219724#gid=1972219724)
#' google sheet.
#'
#' @param use_cases_path The file path to the "Use Cases" page HTML file, as a
#' string.
#' @param table_id The id of the table to update in the HTML file, as a string.
#'
#' @returns The page's update HTML, invisibly.
#'
#' @export
update_website_use_cases <- function(use_cases_path, table_id = "use-cases") {
  stopifnot(file.exists(use_cases_path))

  # read current HTML & table
  html <- readr::read_file(use_cases_path)
  existing_table <- get_html_table(html, table_id)
  table_indent <- get_html_indent(existing_table)

  # get & reformat data
  uc_gs <- googlesheets4::read_sheet(
    ss = .DO_gs$user$ss,
    sheet = .DO_gs$user$sheet
  )

  uc_df <- uc_gs |>
    # drop rows with missing data or w/o checkbox in 'added' column
    dplyr::filter(
      !dplyr::if_any(c("added", "name", "url"), is.na)
    ) |>
    dplyr::select("name", "url", "primary_category") |>
    dplyr::arrange(.data$name) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), stringr::str_trim),
      url = DO.utils::format_hyperlink(
        .data$url,
        as = "html",
        text = .data$name,
        target = "_blank"
      ),
      category_span = stringr::str_split(.data$primary_category, ", *") |>
        purrr::map_chr(
          ~ glue::glue(
            '<span class="badge rounded-pill uc-tag" ',
            'data-category="{category}">{category}</span>',
            category = .x
          ) |>
            paste0(collapse = ", ")
        )
    )

  new_table <- build_datatable_html(
    uc_df,
    col_spec = list(
      "Name" = list(content = "url", search = "name"),
      "Category" = list(
        content = "category_span",
        search = "primary_category"
      )
    ),
    tbl_id = table_id,
    tbl_attr = list(style = "width:100%;", class = "display"),
    header_wrap = c('{{ _("', '") }}'),
    indent = 0L
  )

  out <- html |>
    stringr::str_replace(
      stringr::str_escape(existing_table),
      new_table
    ) |>
    copy_thead_to_tfoot() |>
    add_table_indent(table_indent)

  res <- readr::write_file(out, use_cases_path)
  invisible(out)
}


#' Update Counts in DO Website HTML
#'
#' Directly updates counts listed in the tables on the disease-ontology.org
#' "DO Imports" and "DO Slims" pages using data from a specified release.
#' Changes to these html files should be reviewed and, if correct, committed to
#' the svn repo for deployment.
#'
#' @inheritParams replace_html_counts
#' @param tag The repo tag to extract data from, as a string.
#'
#' @returns
#' Updated counts directly in the html of the svn repo for each page,
#' _as well as_, the old and new counts for comparison as a list of tibbles
#' (invisibly).
#'
#' @export
update_website_count_tables <- function(DO_repo, tag, svn_repo) {
  # reversibly checkout tag
  repo <- git2r::repository(DO_repo)
  repo_head <- git2r::repository_head(repo)
  on.exit(git2r::checkout(repo_head))
  git2r::checkout(repo, tag)

  imports <- replace_html_counts(DO_repo, svn_repo, "imports", reload = TRUE)
  slims <- replace_html_counts(DO_repo, svn_repo, "slims", reload = FALSE)

  invisible(list(imports = imports, slims = slims))
}


#' Update 'Registry of Contributor' Tables on Web Pages
#'
#' Updates data in the 'Registry of Contributor' tables of disease-ontology.org
#' to match the curated contributor data in the
#' [DO_contributors](https://docs.google.com/spreadsheets/d/1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM/)
#' google sheet.
#'
#' @param contrib_path The file path to the "Contributors" page HTML file, as a
#' string.
#' @param table_id The id of the table to update in the HTML file, as a string.
#'
#' @returns The page's updated HTML, invisibly.
#'
#' @export
update_website_contributors <- function(contrib_path, table_id) {
  stopifnot(file.exists(contrib_path))
  table_id_possible <- names(.DO_gs$contributors)
  if (!table_id %in% table_id_possible) {
    stop(
      "Invalid table_id: ",
      table_id,
      ". Must be one of: ",
      paste(table_id_possible, collapse = ", ")
    )
  }

  # read current HTML & table
  html <- readr::read_file(contrib_path)
  existing_table <- get_html_table(html, table_id)
  table_indent <- get_html_indent(existing_table)

  # get & reformat data
  contrib_gs <- googlesheets4::read_sheet(
    ss = .DO_gs$contributors[[table_id]]$ss,
    sheet = .DO_gs$contributors[[table_id]]$sheet,
    col_types = "c"
  )

  brand_regex <- unique_to_string(
    stringr::str_escape(names(brand_fa)),
    delim = "|"
  ) |>
    length_sort(decreasing = TRUE)

  contrib_df <- contrib_gs |>
    dplyr::mutate(
      # add id column (to disambuiguate identical names)
      id = dplyr::row_number(),
      # use GitHub username as name when name is missing
      name = dplyr::if_else(
        !is.na(name),
        .data$name,
        stringr::str_match(.data$github, "github.com/([^/]+)")[, 2]
      )
    ) |>
    tidyr::unite(
      col = "links",
      "github",
      "orcid",
      "other_links",
      sep = "|",
      remove = TRUE,
      na.rm = TRUE
    ) |>
    lengthen_col(cols = "links", delim = "|") |>
    dplyr::mutate(
      # identify link_type (special handling for URLs)
      link_type = stringr::str_extract(
        .data$links,
        stringr::regex(brand_regex, ignore_case = TRUE)
      ),
      link_type = dplyr::if_else(
        is.na(.data$link_type) &
          stringr::str_detect(
            .data$links,
            stringr::regex("^https?://", ignore_case = TRUE),
          ),
        "url",
        .data$link_type
      ),
      link_type = factor(
        .data$link_type,
        levels = c(names(brand_fa), "url")
      ),
      # drop private or un-hyperlinkable links (e.g. emails)
      links = dplyr::if_else(
        !is.na(.data$link_type),
        .data$links,
        NA_character_
      )
    ) |>
    # order links by preference (set by brand_fa order), then alphabetically
    dplyr::arrange(.data$id, .data$link_type, .data$links)

  contrib_df_html <- contrib_df |>
    # generate icon links
    dplyr::mutate(
      icon = to_fa_icon(.data$link_type, size = "fa-xl"),
      # include full URLs when no logo is available
      links = dplyr::case_when(
        .data$link_type == "url" ~ DO.utils::format_hyperlink(
          .data$links,
          as = "html",
          target = "_blank"
        ),
        !is.na(.data$icon) ~ DO.utils::format_hyperlink(
          .data$links,
          as = "html",
          text = .data$icon,
          target = "_blank"
        ),
        .default = NA_character_
      )
    ) |>
    # drop rows with missing name or noted as "exclude"
    dplyr::filter(
      !is.na(.data$name),
      is.na(.data$status) | !stringr::str_detect(.data$status, "exclude")
    ) |>
    dplyr::select("id", "name", "links", "affiliation") |>
    DO.utils::collapse_col(.cols = -"id", delim = ", ") |>
    dplyr::select(-"id") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ stringr::str_replace_all(.x, "\n+", ", ")
      )
    )

  new_table <- build_datatable_html(
    contrib_df_html,
    col_spec = list(
      "Name" = list(content = "name"),
      "Links" = list(content = "links"),
      "Affiliation" = list(content = "affiliation")
    ),
    tbl_id = table_id,
    tbl_attr = list(style = "width:100%;", class = "display"),
    header_wrap = c('{{ _("', '") }}'),
    indent = 0L
  ) |>
    copy_thead_to_tfoot() |>
    add_table_indent(table_indent)

  out <- html |>
    stringr::str_replace(
      stringr::str_escape(existing_table),
      new_table
    )

  readr::write_file(out, contrib_path)
}
