# make_use_case_html() helper
#   sorts list into n cols (integer); fixes the alphabetical ordering from row
#   first to column first.
html_col_sort <- function(x, cols) {
    UseMethod("html_col_sort")
}

#' @export
html_col_sort.data.frame <- function(x, cols) {
    len <- nrow(x)
    idx <- create_row_index(len, cols)
    x %>%
        dplyr::mutate(.row_id = idx) %>%
        dplyr::arrange(.row_id) %>%
        dplyr::select(-.row_id)
}

#' @export
html_col_sort.default <- function(x, cols) {
    idx <- create_row_index(length(x), cols)
    purrr::map(
        1:max(idx),
        function(i) {
            x[idx == i]
        }
    ) %>%
        unlist(recursive = FALSE)
}

# Ensures correct ordering; needed because repeating 1:rows when there
# are < cols - 1 items in the last row causes misordering of last row.
create_row_index <- function(len, cols) {
    in_last_row <- len %% cols
    rows <- round_up(len / cols, 0)
    row_idx <- c(
        rep(1:rows, times = in_last_row),
        rep(1:(rows - 1), times = cols - in_last_row)
    )
    row_idx
}


# update_website_count_tables() helpers -----------------------------------

#' Replace Counts in HTML File
#'
#' Replaces counts in the specified page of disease-ontology.org.
#' `replace_html_counts()` is the primary workhorse supporting the more
#' generalized [update_website_count_tables()].
#'
#' @param DO_repo A `pyDOID.repo.DOrepo` object (see [DOrepo()]).
#' @param svn_repo The local path to the DO website svn directory, as a string.
#'     The correct directory will include a Dockerfile and the
#'     'disease_ontology' directory.
#' @param page The disease-ontology.org in which to replace counts, as a string.
#'     Either "imports" or "slims".
#' @param reload Whether to reload the RDF/OWL file prior to executing a SPARQL
#'     query, as a boolean.
#'
#' @returns
#' Updated counts directly in the html of the svn repo for the specified page,
#' _as well as_, the old and new counts for comparison (invisibly).
#'
#' @family update_website_count_tables() helpers
#' @noRd
replace_html_counts <- function(DO_repo, svn_repo, page, reload = FALSE) {
    page <- match.arg(page, choices = c("imports", "slims"))
    page_path <- switch(
        page,
        imports = file.path(
            svn_repo,
            "disease_ontology/templates/resources/DO_Imports.html"
        ),
        slims = file.path(
            svn_repo,
            "disease_ontology/templates/resources/DO_Slims.html"
        )
    )
    page_html <- readr::read_lines(page_path)

    sparql_dir <- system.file("sparql", package = "DO.utils", mustWork = TRUE)
    query <- switch(
        page,
        imports = file.path(sparql_dir, "website-imports.rq"),
        slims = file.path(sparql_dir, "website-slims.rq")
    )
    data_df <- DO_repo$doid_merged$query(query, reload = reload)
    txt_count <- format(data_df$count, big.mark = ",", trim = TRUE)

    # identify replacement positions in html
    pos <- find_count_pos(page_html, data_df, page)
    warn_missing_pos(pos, data_df, page)

    # capture old count & calculate diff for comparison & validation
    all_html_numbers <- stringr::str_extract(page_html, "[0-9,]+") %>%
        stringr::str_remove_all(",") %>%
        as.integer()
    data_df <- dplyr::mutate(
        data_df,
        old = purrr::map_int(
            pos,
            ~ ifelse(
                .x == 0,
                NA_integer_,
                as.integer(all_html_numbers[.x])
            )
        ),
        new = count,
        count = NULL,
        diff = new - old
    )

    html_out <- page_html
    for (i in seq_along(pos)) {
        html_out[pos[i]] <- stringr::str_replace(
            html_out[pos[i]],
            "[0-9,]+",
            txt_count[i]
        )
    }

    readr::write_lines(html_out, page_path)

    invisible(tibble::as_tibble(data_df))
}

#' Find Positions of Counts in HTML Vector
#'
#' Finds the positions of counts in `page_html`.
#'
#' @param page_html The html of the specified page, as a character vector (from
#'     `readr::read_lines()`.
#' @param data_df The data obtained from querying doid-merged.owl for imports or
#'     slims, as a tibble.
#' @inheritParams replace_html_counts
#'
#' @section NOTE:
#' This approach is somewhat fragile as it assumes the table will maintain 3
#' columns with counts in the third position _and_ that the html for each cell
#' of the table will be on its own line.
#'
#' @family update_website_count_tables() > replace_html_counts() helpers
#' @noRd
find_count_pos <- function(page_html, data_df, page) {
    page <- match.arg(page, choices = c("imports", "slims"))

    if (page == "imports") {
        import_root_recode <- c(
            chebi = "chemicals", omim_susceptibility = "omim", onset = "onset",
            `ontology relations` = "relation", ncbitaxon = "taxon",
            `transmission process` = "transmission"
        )
        id <- dplyr::recode(data_df$import_root, !!!import_root_recode)
    }

    if (page == "slims") {
        id <- data_df$slim
    }

    data_pattern <- paste0("<td>[^/]*", id)
    pos <- purrr::map_dbl(
        data_pattern,
        function(.p) {
            match_pos <- which(stringr::str_detect(page_html, .p)) + 2
            if (length(match_pos) == 0) {
                0
            } else {
                match_pos
            }
        }
    )

    pos
}

#' Warning for Missing Count Positions
#'
#' Warns when data includes imports or slims that are not found in the
#' corresponding table of disease-ontology.org. These indicate a need to review
#' the data for problems or to ensure the website includes all desired counts.
#' The HTML for a missing counts will need to be added manually and, for the
#' Imports page `import_root_recode` in [find_count_pos()] may need to be
#' updated.
#'
#' @param pos The position of counts in an html character vector, as a numeric
#'     vector.
#' @param data_df The data obtained from querying doid-merged.owl for imports or
#'     slims, as a tibble.
#' @inheritParams replace_html_counts,find_count_pos
#'
#' @family update_website_count_tables() > replace_html_counts() helpers
#' @noRd
warn_missing_pos <- function(pos, data_df, page) {
    page <- match.arg(page, choices = c("imports", "slims"))

    if (any(pos == 0) && page == "imports") {
        root_missing <- data_df$import_root[pos == 0]
        names(root_missing) <- rep("i", length(root_missing))
        rlang::warn(
            c("Import(s) exist that are not included in the Imports page count table:",
              root_missing
            )
        )
    }

    if (any(pos == 0) && page == "slims") {
        slim_missing <- data_df$slim[pos == 0]
        names(slim_missing) <- rep("i", length(slim_missing))
        rlang::warn(
            c("Slims(s) exist that are not included in the Slims page count table:",
              slim_missing
            )
        )
    }
}





# Website plot() helpers --------------------------------------------------

# plot_def_src() helper
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", envir = parent.frame())
    df$Source %in% c("ncbi.nlm.nih.gov", "nlm.nih.gov") &
        stringr::str_detect(df$path, subdomain)
}


# plot_citedby() helper
clean_pub_type <- function(pub_type, as_fctr = TRUE) {
    pt_lc <- stringr::str_to_lower(pub_type)
    tidy <- dplyr::case_when(
        stringr::str_detect(pt_lc, "retract") ~ "Retracted",
        stringr::str_detect(pt_lc, "clinical trial") ~ "Clinical Trial",
        stringr::str_detect(pt_lc, "review") ~ "Review",
        stringr::str_detect(pt_lc, "conference") &
            pt_lc != "Journal|Conference Paper" ~ "Conference",
        stringr::str_detect(pt_lc, "book") ~ "Book",
        stringr::str_detect(pt_lc, "journal.*article") ~ "Article",
        TRUE ~ "Commentary"
    )

    if (as_fctr) {
        tidy <- factor(
            tidy,
            # order by least to most important (most important at bottom of graph)
            levels = c("Retracted", "Commentary", "Review", "Conference", "Book",
                       "Clinical Trial", "Article")
        )
    }

    tidy
}

# establish DO theme
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
