#' Make HTML for DO Use Case Tables
#'
#' Makes the row and cell html code for the various sections/tables of the
#' disease-ontology.org "Use Cases" page from the DO team's "DO_uses" google
#' sheet. This function explicitly avoids including the html code for defining
#' the table itself to provide for flexibility. The "html" output in the files
#' specified must be manually copied and pasted into the disease-ontology.org
#' "Use Cases" file in the appropriate section/table.
#'
#' @param out_dir The path to the directory where output should be saved, as a
#'     string.
#' @param group The group(s) to generate html for, as a character vector. One or
#'      more of: "all" (default), "ontology", "resource", or "methodology".
#'
#' @returns
#' One "html" file in `out_dir` for each `group` named as
#' "DO_use_case-\{group\}.html" and the "User" data from the Google Sheet
#' invisibly.
#'
#' @export
make_use_case_html <- function(out_dir = "graphics/website", group = "all") {
    # validate arguments
    if (!rlang::is_string(out_dir) || !dir.exists(out_dir)) {
        rlang::abort(
            message = "`out_dir` is not a single directory or does not exist."
        )
    }
    possible_use_cases <- c("ontology", "resource", "methodology")
    group <- match.arg(group, c("all", possible_use_cases), several.ok = TRUE)
    if ("all" %in% group) {
        group <- possible_use_cases
    }

    out_file <- file.path(out_dir, paste0("DO_use_case-", group, ".html"))

    # prep data
    use_case_gs <- googlesheets4::read_sheet(
        ss = .DO_gs$users,
        sheet = "DO_website_user_list",
        range = "A:E",
        col_types = "lcccc"
    )
    use_case_df <- use_case_gs %>%
        dplyr::filter(!is.na(.data$added))

    use_case_list <- purrr::map(
        group,
        ~ dplyr::filter(use_case_df, .data$type == .x) %>%
            # ensure use cases are alphabetical by column
            dplyr::arrange(.data$name) %>%
            html_col_sort(3)
    ) %>%
        purrr::set_names(nm = group)

    # build html
    use_case_html_list <- purrr::map(
        use_case_list,
        function(.df) {
            glue::glue_data(
                .x = .df,
                '<a href="{url}" target="_blank">{name}</a>'
            ) %>%
                html_in_rows(
                    per_row = 3,
                    indent_n = 2,
                    cell_attr = c(class="default")
                )
        }
    )

    # save files
    purrr::walk2(
        .x = use_case_html_list,
        .y = out_file,
        ~ readr::write_lines(x = .x, file = .y)
    )

    invisible(use_case_gs)
}


#' Update Counts in DO Website HTML
#'
#' Directly updates counts listed in the tables on the disease-ontology.org
#' "DO Imports" and "DO Slims" pages using data from a specified release.
#' Changes to these html files should be reviewed and, if correct, committed to
#' the svn repo for deployment.
#'
#' @param DO_repo A `pyDOID.repo.DOrepo` object (see [DOrepo()]).
#' @param tag The repo tag to extract data from, as a string.
#' @param svn_repo The local path to the DO website svn directory, as a string.
#'     The correct directory will include a Dockerfile and the
#'     'disease_ontology' directory.
#'
#' @returns
#' Updated counts directly in the html of the svn repo for each page,
#' _as well as_, the old and new counts for comparison as a list of tibbles
#' (invisibly).
#'
#' @section Speed Note:
#' Expect this function to make more than a minute. The majority of the time is
#' consumed loading the doid-merged.owl file and cannot be sped up
#' without a faster RDF/OWL parser. This is currently handled via DO.utils'
#' dependency on the python package pyDOID with RDF handled by the RDFLib python
#'  package.
#'
#' @export
update_website_count_tables <- function(DO_repo, tag, svn_repo) {
    # validate arguments
    if (class(DO_repo)[1] != "pyDOID.repo.DOrepo") {
        rlang::abort("`DO_repo` is not a pyDOID.repo.DOrepo object.")
    }
    if (!rlang::is_string(tag)) {
        rlang::abort("`tag` must be a string corresponding to a DO release.")
    }
    if (!rlang::is_string(svn_repo) || !dir.exists(svn_repo)) {
        rlang::abort(
            message = "`svn_repo` is not a single directory or does not exist."
        )
    }

    # reversibly checkout tag; tmp to capture empty lines in output
    DO_repo$capture_head()
    on.exit(DO_repo$restore_head())
    DO_repo$checkout_tag(tag)

    imports <- replace_html_counts(DO_repo, svn_repo, "imports", reload = TRUE)
    slims <- replace_html_counts(DO_repo, svn_repo, "slims", reload = FALSE)

    invisible(list(imports = imports, slims = slims))
}


#' Make HTML for DO User List (DEPRECATED)
#'
#' Makes the row and cell html code for the "Users of the Disease Ontology"
#' section of the collaborators page on disease-ontology.org from the DO team's
#' "Uses" google sheet. This function explicitly avoids including the html
#' code for defining the table itself to provide for flexibility.
#'
#' @param file The file path where the output should be saved, as a string.
#'
#' @section Deprecation Notice:
#' The information this was formatting for disease-ontology.org was moved from
#' the "Collaborators" page to the new "Use Cases" page in mid-2022 and was
#' split from one section into three, making this function obsolete. Use
#' [make_use_case_html()] instead.
#'
#' @export
make_user_list_html <- function(file) {
    continue <- NA
    while (!continue %in% c("y", "n")) {
        continue <- readline("This function has been deprecated. Would you like to continue anyway? y/n")
        cotinue <- stringr::str_to_lower(continue)
    }
    if (continue == "n") {
        message("Use make_use_case_html() instead.")
        return(invisible())
    }

    # get data
    user_list <- googlesheets4::read_sheet(
        ss = .DO_gs$users,
        sheet = "DO_website_user_list",
        range = "A:E",
        col_types = "lcccc"
    )
    ws_user_list <- user_list %>%
        dplyr::filter(!is.na(.data$added)) %>%
        # ensure list is alphabetical
        dplyr::arrange(.data$name)

    # build html
    user_html <- glue::glue_data(
        .x = ws_user_list,
        '<a href="{url}" target="_blank">{name}</a>'
    )
    html_rows <- html_in_rows(user_html, per_row = 3, indent_n = 2,
                              cell_attr = c(class="default"))

    readr::write_lines(html_rows, file = file)
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
                txt = "Github"
            ),
            orcid = build_hyperlink(
                x = .data$orcid,
                url = "orcid",
                as = "html",
                txt = "ORCID"
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
