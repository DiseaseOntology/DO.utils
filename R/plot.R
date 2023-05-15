#' Plot Branch Counts
#'
#' Plots the count of _non-obsolete_ terms in each major branch of the Human
#' Disease Ontology.
#'
#' @param DO_repo The local path to the HumanDiseaseOntology repo, as a string,
#'     or a [DOrepo] object.
#' @param out_dir The directory where the plot `"DO_branch_count.png"`
#'     should be saved, as a string. If `NULL`, the plot is not saved to disk.
#' @param w The width of the plot in inches, as numeric.
#' @param h The height of the plot in inches, as numeric.
#' @param aspect_ratio The aspect ratio of the panel (i.e. plot area), as
#'     numeric. If `NULL`, the aspect ratio will not be set.
#'
#' @export
plot_branch_counts <- function(DO_repo, out_dir = "graphics/website",
                               w = 8, h = 5.6, aspect_ratio = 1) {
    DO_repo <- access_DOrepo(DO_repo)
    branch_query <- system.file(
        "sparql/branch-count.rq",
        package = "DO.utils",
        mustWork = TRUE
    )

    asserted <- DO_repo$doid_non_classified$query(branch_query) %>%
        tidy_sparql() %>%
        dplyr::rename("Asserted" = "count")
    total <- DO_repo$doid$query(branch_query) %>%
        tidy_sparql() %>%
        dplyr::rename("Total" = "count")
    df <- dplyr::left_join(asserted, total, by = "branch") %>%
        dplyr::mutate(
            Inferred = .data$Total - .data$Asserted,
            branch = stringr::str_to_title(
                stringr::str_remove(.data$branch, "disease (of|by) ")
            )
        ) %>%
        tidyr::pivot_longer(
            cols = c("Asserted", "Inferred"),
            names_to = "class_type",
            values_to = "Count"
        ) %>%
        dplyr::mutate(
            branch = factor(
                .data$branch,
                levels = rev(
                    c("Infectious Agent", "Anatomical Entity",
                      "Cellular Proliferation", "Mental Health","Metabolism",
                      "Genetic Disease", "Physical Disorder", "Syndrome")
                )
            ),
            class_type = factor(
                .data$class_type,
                levels = c("Inferred", "Asserted")
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(
                x = .data$branch,
                y = .data$Count,
                fill = .data$class_type
            ),
            width = 0.6,
            position = "stack"
        ) +
        ggplot2::scale_fill_manual(
            name = "Class",
            values = unname(DO_colors[c("sat_light", "sat")])
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            limits = c(0, round_up(max(df$Total), -3)),
            expand = ggplot2::expansion(0, 0)
        ) +
        ggplot2::coord_flip() +
        theme_DO(base_size = 13) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    if(!is.null(aspect_ratio)) {
        g <- g +
            ggplot2::theme(aspect.ratio = aspect_ratio)
    }

    if (!is.null(out_dir)) {
        file_out <- file.path(out_dir, "DO_branch_count.png")

        ggplot2::ggsave(
            filename = file_out,
            plot = g,
            width = w, height = h, units = "in",
            dpi = 600
        )
    }

    g
}


#' Plot Publications Citing DO by Year
#'
#' Plots the count of publications that cite the Human Disease Ontology by
#' year.
#'
#' @param data_file The path to the file containing the list of publications
#'     citing the DO, as a string.
#' @param out_dir The directory where the plot `"DO_cited_by_count.png"`
#'     should be saved, as a string. If `NULL` the plot is not saved to disk.
#' @param color_set The prefix of the color set to use from [DO_colors].
#'     Available sets include: "sat", "accent1" (default), "accent2", and
#'     "orange".  The default and light versions of the specified color set will
#'     be used.
#' @inheritParams plot_branch_counts
#'
#' @section Data Preparation:
#' To prepare data, execute `scripts/citedby_full_procedure.R`.
#'
#' @export
plot_citedby <- function(data_file = "data/citedby/DO_citedby.csv",
                         out_dir = "graphics/website", color_set = "accent1",
                         w = 8, h = 5.6) {

    df <- readr::read_csv(data_file) %>%
        dplyr::mutate(
            Year = lubridate::year(.data$pub_date),
            pub_type = clean_pub_type(.data$pub_type)
        )

    # set color ramp
    cb_colors <- grDevices::colorRampPalette(
        DO_colors[paste0(color_set, c("_light", ""))]
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

    if (!is.null(out_dir)) {
        file_out <- file.path(out_dir, "DO_cited_by_count.png")

        ggplot2::ggsave(
            filename = file_out,
            plot = g,
            width = w,
            height = h,
            units = "in",
            dpi = 600
        )
    }

    g
}


#' Plot Definition Sources
#'
#' Plots the count of definition sources for _non-obsolete_ terms the Human
#' Disease Ontology.
#'
#' @inheritParams read_doid_edit
#' @param out_dir The directory where the plot "DO_def_src.png" should be saved,
#'     as a string. If `NULL` the plot is not saved to disk.
#' @inheritParams plot_branch_counts
#'
#' @section Data Preparation:
#' If this plot will be added to disease-ontology.org, the latest release of the
#' `HumanDiseaseOntology` Github repo should be checked out prior to running
#' this function.
#'
#' @export
plot_def_src <- function(DO_repo, out_dir = "graphics/website",
                         w = 8, h = 5.6) {
    df <- read_doid_edit(DO_repo) %>%
        extract_doid_url() %>%
        dplyr::mutate(domain = extract_url_domain(.data$url, drop_www = TRUE)) %>%
        # tidy source names
        dplyr::mutate(
            Source = stringr::str_to_lower(.data$domain),
            Source = dplyr::recode(
                .data$Source,
                # malformed/not working/need replacement
                "pubmed-ncbi-nlm-nih-gov" = "PubMed",
                "ncithesaurus-stage.nci.nih.gov" = "NCI Thesaurus",
                "ncim.nci.nih.gov" = "NCI Thesaurus",
                "bt.cdc.gov" = "CDC",
                # redirects
                "mayoclinic.com" = "Mayo Clinic",
                "nci.nih.gov" = "NCI",
                "cancergenome.nih.gov" = "NCI",
                "dpd.cdc.gov" = "CDC",
                "springerlink.com" = "link.springer.com",
                "ghr.nlm.nih.gov" = "MedlinePlus",
                # general tidying
                "apps.who.int" = "WHO",
                "whqlibdoc.who.int" = "WHO",
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
            limits = c(0, round_up(max(plot_df$Count), -3)),
            expand = ggplot2::expansion(0, 0)
        ) +
        ggplot2::coord_flip() +
        theme_DO(base_size = 13) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())

    if (!is.null(out_dir)) {
        file_out <- file.path(out_dir, "DO_def_src.png")

        ggplot2::ggsave(
            filename = file_out, plot = g,
            width = w, height = h, units = "in",
            dpi = 600
        )
    }

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
#' @param out_dir The directory where the plot `"DO_term_def_count.png"`
#'     should be saved, as a string. If `NULL` the plot is not saved to disk.
#' @inheritParams plot_branch_counts
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
        dplyr::select(-.data$defs) %>%
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
    g <- df %>%
        dplyr::select(-"terms") %>%
        ggplot2::ggplot() +
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
            limits = c(0, round_up(max(df$terms), -3)),
            expand = ggplot2::expansion(0, 0)
        ) +
        ggplot2::scale_x_date(
            name = "Release Date",
            date_labels = "%Y"
        ) +
        theme_DO(base_size = 13)

    if (!is.null(out_dir)) {
        file_out <- file.path(out_dir, "DO_term_def_count.png")

        ggplot2::ggsave(
            filename = file_out, plot = g,
            width = w, height = h, units = "in",
            dpi = 600
        )
    }

    g
}


#' Plot Xref Counts
#'
#' Plots the count of cross-references by source in the Human Disease Ontology.
#'
#' @inheritParams plot_branch_counts
#' @param out_dir The directory where the plot `"DO_xref_count.png"`
#'     should be saved, as a string. If `NULL` the plot is not saved to disk.
#'
#' @export
plot_xref_counts <- function(DO_repo, out_dir = "graphics/website",
                             w = 8, h = 5.6) {
    DO_repo <- access_DOrepo(DO_repo)
    xref_query <- system.file(
        "sparql/all-xref-report.rq",
        package = "DO.utils",
        mustWork = TRUE
    )

    df <- DO_repo$doid$query(xref_query) %>%
        tidy_sparql() %>%
        dplyr::mutate(
            prefix = stringr::str_remove(
                .data$prefix,
                "_[0-9]{4}_[0-9]{2}_[0-9]{2}"
            )
        ) %>%
        dplyr::count(.data$prefix, wt = .data$count, name = "count")

    curation_type <- c(
        ORDO = "Manual", OMIM = "Manual", MEDDRA = "Manual", KEGG = "Manual",
        ICDO = "Manual", GARD = "Manual", EFO = "Manual", ICD11 = "Manual",
        NCI = "Mixed", MESH = "Mixed",
        UMLS_CUI = "Automated", SNOMEDCT_US = "Automated", ICD9CM = "Automated",
        ICD10CM = "Automated"
    )

    df <- df %>%
        dplyr::mutate(
            Curation = factor(
                dplyr::recode(.data$prefix, !!!curation_type),
                levels = c("Manual", "Mixed", "Automated")
            ),
            prefix = dplyr::recode(
                .data$prefix,
                MESH = "MeSH", NCI = "NCIt", MEDDRA = "MedDRA",
                ORDO = "Orphanet", UMLS_CUI = "UMLS"
            )
        )

    g <- ggplot2::ggplot(data = df) +
        ggplot2::geom_col(
            ggplot2::aes(
                x = .data$prefix,
                y = .data$count,
                fill = .data$Curation
            ),
            width = 0.6
        ) +
        ggplot2::scale_y_continuous(
            name = NULL,
            limits = c(0, round_up(max(df$count), -3)),
            expand = ggplot2::expansion(0, 0)
        ) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(
            values = unname(DO_colors[c("sat_light", "sat_mid", "sat")])
        ) +
        ggplot2::labs(x ="Cross References") +
        theme_DO(base_size = 13)

    if (!is.null(out_dir)) {
        file_out <- file.path(out_dir, "DO_xref_count.png")

        ggplot2::ggsave(
            filename = file_out, plot = g,
            width = w, height = h, units = "in",
            dpi = 600
        )
    }

    g
}


#' DO Theme for Stats Plots
#'
#' The default ggplot2 theme used for statistical plots uploaded to
#' disease-ontology.org. This _only_ manages the base plot style and does _not_
#' incorporate DO's color scheme.
#'
#' @inheritParams ggplot2::theme_grey
#'
#' @section Background:
#' For more information refer to ggplot2's
#' [theme documentation](ggplot2::theme_grey()).
#'
#' @export
theme_DO <- function(base_size = 11, base_family = "",
                     base_line_size = base_size/22,
                     base_rect_size = base_size/22) {
    half_line <- base_size/2
    ggplot2::`%+replace%`(
        ggplot2::theme_grey(
            base_size = base_size, base_family = base_family,
            base_line_size = base_line_size, base_rect_size = base_rect_size
        ),
        ggplot2::theme(
            panel.background = ggplot2::element_rect(fill = "grey60", colour = NA),
            panel.grid = ggplot2::element_line(colour = "grey50"),
            panel.grid.major = ggplot2::element_line(size = ggplot2::rel(0.3)),
            panel.grid.minor = ggplot2::element_line(size = ggplot2::rel(0.15)),
            axis.ticks = ggplot2::element_line(
                colour = "grey40",
                size = ggplot2::rel(0.3)
            ),
            legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(
                fill = "grey20",
                colour = NA
            ),
            strip.text = ggplot2::element_text(
                colour = "grey90",
                size = ggplot2::rel(0.8),
                margin = ggplot2::margin(
                    0.8 * half_line,
                    0.8 * half_line,
                    0.8 * half_line,
                    0.8 * half_line
                )
            ),
            complete = TRUE
        )
    )
}
