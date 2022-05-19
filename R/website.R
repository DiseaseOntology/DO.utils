#' Make HTML for DO Use Case Tables
#'
#' Makes the row and cell html code for the various sections/tables of the
#' disease-ontology.org "Use Cases" page from the DO team's "Uses" google sheet.
#' This function explicitly avoids including the html code for defining the
#' table itself to provide for flexibility. The "html" output in the files
#' specified should be copied and pasted into the disease-ontology.org
#' "Use Cases" file in the appropriate section/table.
#'
#' @param out_dir The path to the directory where output should be saved, as a
#'     string.
#' @param .which The list(s) to generate, as a character vector. One or more of:
#'     "all" (default, to make all lists), "ontology", "resource", or
#'     "methodology".
#'
#' @returns
#' One "html" file for each group in `.which` named as
#' "\{datestamp\}-\{.which\}.html".
#'
#' Also returns the "User" data from the Google Sheet invisibly.
#'
#' NOTE: The "html" is incomplete and cannot be loaded by browsers,
#' as is.
#'
#' @export
make_use_case_html <- function(out_dir, .which = "all") {
    # validate arguments
    if (!rlang::is_string(out_dir) || !dir.exists(out_dir)) {
        rlang::abort(
            message = "`out_dir` is not a single directory or does not exist."
        )
    }
    possible_use_cases <- c("ontology", "resource", "methodology")
    .which <- match.arg(.which, c("all", possible_use_cases), several.ok = TRUE)
    if ("all" %in% .which) {
        .which <- possible_use_cases
    }

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
        .which,
        ~ dplyr::filter(use_case_df, .data$type == .x) %>%
            # ensure use cases are alphabetical by column
            dplyr::arrange(.data$name) %>%
            html_col_sort(3)
    ) %>%
        purrr::set_names(nm = .which)

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
    out_file <- file.path(
        out_dir,
        paste0(today_datestamp(), "-", .which, ".html")
    )
    purrr::walk2(
        .x = use_case_html_list,
        .y = out_file,
        ~ readr::write_lines(x = .x, file = .y)
    )

    invisible(use_case_gs)
}


#' Make HTML for DO User List
#'
#' Makes the row and cell html code for the "Users of the Disease Ontology"
#' section of the collaborators page on disease-ontology.org from the DO team's
#' "Uses" google sheet. This function explicitly avoids including the html
#' code for defining the table itself to provide for flexibility.
#'
#' @param file The file path where the output should be saved, as a string.
#'
#' @export
make_user_list_html <- function(file) {
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



# Statistics Plots --------------------------------------------------------

#' Plot Publications Citing DO by Year
#'
#' Plots the count of publications that cite the Human Disease Ontology by
#' year.
#'
#' @param data_file The path to the file containing the list of publications
#'     citing the DO, as a string.
#' @param out_dir The directory where the plot `"{date}-DO_cited_by_count.png"`
#'     should be saved, as a string.
#' @param w The width of the plot in inches, as an integer.
#' @param h The height of the plot in inches, as an integer.
#'
#' @section Data Preparation:
#' To prepare data, execute `scripts/citedby_full_procedure.R`.
#'
#' @export
plot_citedby <- function(data_file = "data/citedby/DO_citedby.csv",
                         out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_cited_by_count.png"
        )
    )

    df <- readr::read_csv(data_file) %>%
        dplyr::mutate(
            Year = lubridate::year(.data$pub_date),
            pub_type = clean_pub_type(.data$pub_type)
        )

    # set color ramp
    cb_colors <- grDevices::colorRampPalette(
        DO_colors[c("sat", "sat_light")]
    )(dplyr::n_distinct(df$pub_type))


    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_bar(
            ggplot2::aes(x = .data$Year, fill = .data$pub_type),
            width = 0.8,
            position = "stack"
        ) +
        ggplot2::scale_fill_manual(
            values = cb_colors,
            name = "Publication Type",
            guide = ggplot2::guide_legend(reverse = TRUE)
        ) +
        ggplot2::labs(x = "Year", y = NULL) +
        theme_DO(base_size = 13)

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot DO Term & Definition Counts
#'
#' Plots the count of _non-obsolete_ terms and definitions in the Human Disease
#' Ontology over time (using data from each release).
#'
#' @param release_file The path to the file containing DO release details, as a
#'     string.
#' @param counts_file The path to the file containing the count of DO terms
#'     and definitions by release, as a string.
#' @param out_dir The directory where the plot `"{date}-DO_term_def_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, execute:
#'
#' 1. `scripts/DO_term_def_counts.R` - requires installation of a python virtual
#'  environment using `scripts/install_reticulate_python.R`.
#'
#' 2. `scripts/DO_release_details.R`
#'
#' @export
plot_term_def_counts <- function(
    release_file = "data/DO_release/DO_release_details.csv",
    counts_file = "data/DO_release/DO_term_def_counts.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_term_def_count.png"
        )
    )

    release_df <- readr::read_csv(release_file)
    counts_df <- readr::read_csv(counts_file) %>%
        dplyr::rename(release = .data$tag_name)
    df <- dplyr::left_join(
        release_df,
        counts_df,
        by = c("tag_name" = "release")
    ) %>%
        # add year
        dplyr::mutate(date = lubridate::date(.data$created_at)) %>%
        # drop bug fix releases that happen on same day (for plotting by date)
        dplyr::group_by(.data$date) %>%
        dplyr::arrange(dplyr::desc(.data$created_at)) %>%
        dplyr::filter(!duplicated(.data$date)) %>%
        dplyr::ungroup() %>%
        # drop extra columns
        dplyr::select(.data$date, .data$terms, .data$defs) %>%
        dplyr::mutate(
            n_terms = .data$terms - .data$defs,
            n_defs = .data$defs
        ) %>%
        dplyr::select(-.data$terms, -.data$defs) %>%
        tidyr::pivot_longer(
            cols = c(.data$n_terms, .data$n_defs),
            names_to = "variable",
            values_to = "value"
        ) %>%
        dplyr::mutate(
            variable = factor(
                .data$variable,
                levels = c("n_terms", "n_defs")
            )
        )

    ## Create Area Plot - NEW version, 2021-08-11
    g <- ggplot2::ggplot(df) +
        ggplot2::geom_area(
            ggplot2::aes(x = .data$date, y = .data$value, fill = .data$variable),
            size = 1
        ) +
        ggplot2::scale_fill_manual(
            name = "Total",
            values = unname(DO_colors[c("sat_light", "sat")]),
            labels = c("Terms", "Terms Defined")
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = seq(0, 12000, by = 2000)
        ) +
        ggplot2::scale_x_date(
            name = "Release Date",
            date_breaks = "1 year",
            date_labels = "%Y"
        ) +
        theme_DO(base_size = 13)

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot Branch Counts
#'
#' Plots the count of _non-obsolete_ terms in each major branch of the Human
#' Disease Ontology.
#'
#' @param data_file The path to the file containing the latest DO branch counts,
#'     as a string.
#' @param out_dir The directory where the plot `"{date}-DO_branch_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, manually copy and paste stats from the Google Sheet
#' [DO_github_release_log](https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
#' or from `build/reports/branch-count.tsv` in the local repo where the release
#' was generated.
#'
#' @export
plot_branch_counts <- function(
    data_file = "data/DO_release/branch_counts.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_branch_count.png"
        )
    )

    branch_order <- c(syndrome = "Syndrome",
                      physicalDisorder = "Physical Disorder",
                      genetic = "Genetic Disease",
                      metabolism = "Metabolism",
                      mentalHealth = "Mental Health",
                      cellularProliferation = "Cellular Proliferation",
                      anatomicalEntity = "Anatomical Location",
                      infectiousAgent = "Infectious Disease")

    df <- readr::read_csv(data_file) %>%
        dplyr::mutate(
            DO_branches = factor(
                dplyr::recode(.data$DO_branches, !!!branch_order),
                levels = branch_order
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data$DO_branches, y = .data$Count),
            width = 0.6, fill = DO_colors["sat_light"]
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = seq(0, round_up(max(df$Count), -3), by = 1000)
        ) +
        ggplot2::coord_flip() +
        theme_DO(base_size = 13) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot Xref Counts
#'
#' Plots the count of cross-references by source in the Human Disease Ontology.
#'
#' @param data_file The path to the file containing the latest DO xref counts,
#'     as a string.
#' @param out_dir The directory where the plot `"{date}-DO_xref_count.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' To prepare data, manually copy and paste stats from the Google Sheet
#' [DO_github_release_log](https://docs.google.com/spreadsheets/d/1-ZSUH43MJloR2EsBqHpGeY6IfKG7Gt8KBcU5remnoGI/edit#gid=269344614)
#' OR manually copy & paste stats from xref.tsv produced by executing:
#'
#' ```
#' robot query --input {DO_repo}/src/ontology/doid.owl \
#'             --query {DO_repo}/src/sparql/build/all-xref-report.rq xref.tsv
#'```
#'
#' @export
plot_xref_counts <- function(
    data_file = "data/DO_release/cross_references.csv",
    out_dir = "graphics/website", w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_xref_count.png"
        )
    )

    df <- readr::read_csv(data_file) %>%
        dplyr::filter(!is.na(.data$Curation)) %>%
        dplyr::mutate(
            Curation = factor(
                .data$Curation,
                levels = c("Manual", "Mixed", "Automated")
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(x = .data$Cross_References, y = .data$Count,
                         fill = .data$Curation),
            width = 0.6
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = seq(0, round_up(max(df$Count), -3), by = 2000)
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
            values = unname(DO_colors[c("sat_light", "sat_mid", "sat")])
        ) +
        ggplot2::labs(x ="Cross References") +
        theme_DO(base_size = 13)

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}


#' Plot Definition Sources
#'
#' Plots the count of definition sources for _non-obsolete_ terms the Human
#' Disease Ontology.
#'
#' @inheritParams read_doid_edit
#' @param out_dir The directory where the plot `"{date}-DO_def_src.png"`
#'     should be saved, as a string.
#' @inheritParams plot_citedby
#'
#' @section Data Preparation:
#' If this plot will be added to disease-ontology.org, the latest release of the
#' `HumanDiseaseOntology` Github repo should be checked out prior to running
#' this function.
#'
#' @export
plot_def_src <- function(DO_repo, out_dir = "graphics/website",
                               w = 8, h = 5.6) {

    file_out <- file.path(
        out_dir,
        paste0(
            stringr::str_remove_all(Sys.Date(), "-"),
            "-",
            "DO_def_src.png"
        )
    )

    df <- read_doid_edit(DO_repo) %>%
        extract_doid_url() %>%
        dplyr::mutate(tmp = parse_url(.data$url)) %>%
        tidyr::unnest(.data$tmp, keep_empty = TRUE) %>%
        # tidy source names
        dplyr::mutate(
            # strip start www and all variants & make lowercase
            Source = stringr::str_remove(.data$domain, "^www[^.]*\\."),
            Source = stringr::str_to_lower(.data$Source),
            # temporary fix for some malformed/old web domains
            Source = dplyr::recode(
                .data$Source,
                # malformed/not working
                "pubmed-ncbi-nlm-nih-gov" = "PubMed",
                "ncithesaurus-stage.nci.nih.gov" = "NCI thesaurus",
                "bt.cdc.gov" = "CDC",
                # redirects
                "mayoclinic.com" = "Mayo Clinic",
                "nci.nih.gov" = "cancer.gov",
                "cancergenome.nih.gov" = "cancer.gov",
                "dpd.cdc.gov" = "CDC",
                "springerlink.com" = "link.springer.com",
                "ghr.nlm.nih.gov" = "MedlinePlus",
                # general tidying
                "apps.who.int" = "who.int",
                "whqlibdoc.who.int" = "who.int",
                "ncithesaurus.nci.nih.gov" = "NCI Thesaurus",
                "pubmed.ncbi.nlm.nih.gov" = "PubMed",
                "pubmedcentral.nih.gov" = "PubMed Central",
                "cdc.gov" = "CDC",
                "en.wikipedia.org" = "Wikipedia",
                "medlineplus.gov" = "MedlinePlus",
                "ncit.nci.nih.gov" = "NCI Thesaurus",
                "omim.org" = "OMIM",
                "mayoclinic.org" = "Mayo Clinic",
                "rarediseases.org" = "NORD",
                "rarediseases.info.nih.gov" = "GARD",
                "cancer.gov" = "NCI"
            )
        ) %>%
        # separate mutate needed for is_nlm_subdomain() to get improved df data
        dplyr::mutate(
            Source = dplyr::case_when(
                stringr::str_detect(.data$url, "mesh") ~ "MeSH",
                is_nlm_subdomain("medlineplus") ~ "MedlinePlus",
                is_nlm_subdomain("pmc") ~ "PubMed Central",
                is_nlm_subdomain("pubmed") ~ "PubMed",
                is_nlm_subdomain("entrez") ~ "PubMed",
                is_nlm_subdomain("omim") ~ "OMIM",
                is_nlm_subdomain("book") ~ "NCBI Bookshelf",
                TRUE ~ Source
            )
        )

    count_df <- dplyr::count(df, .data$Source, name = "Count", sort = TRUE) %>%
        dplyr::mutate(rank = dplyr::row_number(dplyr::desc(.data$Count)))

    total_url <- sum(count_df$Count)
    top_10 <- count_df %>%
        dplyr::filter(rank <= 10)
    other <- count_df %>%
        dplyr::filter(rank > 10) %>%
        dplyr::summarize(
            Source = paste0(
                "Other Sources (",
                dplyr::n_distinct(.data$Source),
                ")"
            ),
            Count = sum(.data$Count),
            rank = 11
        )
    plot_df <- dplyr::bind_rows(top_10, other)

    y_axis_max <- round_up(max(plot_df$Count), -3)
    g <- ggplot2::ggplot(data = plot_df) +
        ggplot2::geom_col(
            ggplot2::aes(
                x = stats::reorder(.data$Source, -.data$rank),
                y = .data$Count
            ),
            width = 0.6, fill = DO_colors["sat_light"]
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            breaks = seq(0, y_axis_max, by = 1000),
        ) +
        ggplot2::coord_flip() +
        theme_DO(base_size = 13) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    ggplot2::ggsave(
        filename = file_out, plot = g,
        width = w, height = h, units = "in",
        dpi = 600
    )

    g
}
