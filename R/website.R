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


#' Make "Contributors" HTML
#'
#' Makes the "Contributor" `<li>` elements for disease-ontology.org. Can be used
#' for any ontology given the appropriate input.
#'
#' @param contrib_df A data.frame with information about contributors, including
#' the required columns: 'name', 'team_member', 'github', and 'orcid'. 'github'
#' and 'orcid' columns can have data missing but at least one should be present
#' for each contributor.
#'
#' @examplesIf interactive()
#' trans_contributors <- googlesheets4::read_sheet(
#'     ss = "1kD7rgOWO2uVUwKYoKFSLBEpv1WZFf-GDhEusAq_H5sM",
#'     sheet = "TRANS",
#'     col_types = "c"
#' ) %>%
#'     dplyr::mutate(dplyr::across(dplyr::everything(), readr::parse_guess))
#' trans_contributors
#'
#' make_contributors_html(trans_contributors)
#'
#' @export
make_contributor_html <- function(contrib_df) {
    .data <- contrib_df %>%
        dplyr::mutate(
            github = build_hyperlink(
                x = .data$github,
                url = "github",
                as = "html",
                text = "Github"
            ),
            orcid = build_hyperlink(
                x = .data$orcid,
                url = "orcid",
                as = "html",
                text = "ORCID"
            ),
            links = purrr::map2_chr(
                .data$github,
                .data$orcid,
                ~ vctr_to_string(c(.x, .y), delim = ", ", na.rm = TRUE)
            )
        )

    member <- dplyr::filter(.data, .data$team_member) %>%
        dplyr::arrange(.data$name)
    member_html <- glue::glue_data(.x = member, "<li>{name} ({links})</li>")

    nonmember <- dplyr::filter(.data, !.data$team_member) %>%
        dplyr::arrange(.data$name)
    nonmember_html <- glue::glue_data(.x = nonmember, "<li>{name} ({links})</li>")

    list(member = member_html, nonmember = nonmember_html)
}
