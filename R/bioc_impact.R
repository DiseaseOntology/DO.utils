#' Get Bioconductor Package Statistics
#'
#' Get download statistics for specific Bioconductor packages.
#'
#' @param pkg character vector; name of Bioconductor package(s)
#' @param pkg_type character vector of same length as `pkg` identifying the type
#'  of Bioconductor package; one of "software", "annotation", "experiment", or
#'  "workflow"
#' @param yr character scalar; 4-digit year of stats desired,
#'  default = current year (currently implemented only for single yr)
#' @param delay_rng `c(min, max)` number of seconds to wait between requests;
#' default = `c(1, 10)`
#'
#' @export
get_bioc_pkg_stats <- function(pkg, pkg_type, yr, delay_rng) {

    if (missing(yr)) {
        yr <- cur_yr()
    }

    pkg_stat_url <- build_bioc_pkg_stat_url(
        pkg = pkg,
        pkg_type = pkg_type,
        yr = yr
    )

    if (length(pkg) > 1) {

        if (missing(delay_rng)) {
            delay_rng <- c(1,10)
        }
        assertthat::assert_that(
            length(delay_rng) == 2,
            is.numeric(delay_rng)
        )

        pkg_stats <- purrr::pmap_dfr(
            .l = list(pkg, pkg_type, pkg_stat_url),
            function(pkg, pkg_type, pkg_stat_url) {
                df <- get_bioc_pkg_stats_(pkg, pkg_type, pkg_stat_url)

                Sys.sleep(
                    runif(1, min = min(delay_rng), max = max(delay_rng))
                )

                df
            }
        )
    } else {
        pkg_stats <- get_bioc_pkg_stats_(pkg, pkg_type, pkg_stat_url)
    }

    pkg_stats
}

get_bioc_pkg_stats_ <- function(pkg, pkg_type, url) {

    assertthat::assert_that(rlang::is_scalar_character(pkg))
    assertthat::assert_that(
        rlang::is_scalar_character(pkg_type),
        pkg_type %in% bioc_pkg_types
    )

    df <- readr::read_tsv(url) %>%
        dplyr::mutate(
            pkg = pkg,
            pkg_type = pkg_type
        ) %>%
        dplyr::select(pkg, pkg_type, everything())

    df
}

build_bioc_pkg_stat_url <- function(pkg, pkg_type, yr) {
    assertthat::assert_that(is_scalar_character(yr))

    type_base_url <- recode(pkg_type, !!!bioc_stat_baseurl)
    pkg_stat_url <- paste0(
        type_base_url, pkg, "/", pkg, "_", yr, "_stats.tab"
    )

    pkg_stat_url
}


# DO R packages
DO_dep_pkg <- list(software = "DOSE", annotation = "DO.db")

bioc_pkg_types <- c("software", "annotation", "experiment", "workflow")

# list bioconductor statistics web resources - main site, base urls,
#   statistics dump file, download score dump file
#
#   * download score = average downloads over 12 month period
bioc_stat_sites <- purrr::set_names(
        c(
            "https://bioconductor.org/packages/stats/",
            "https://bioconductor.org/packages/stats/data-annotation.html",
            "https://bioconductor.org/packages/stats/data-experiment.html",
            "https://bioconductor.org/packages/stats/workflows.html"
        ),
        bioc_pkg_types
    )

bioc_stat_baseurl <- purrr::set_names(
    c(
        "https://bioconductor.org/packages/stats/bioc/",
        "https://bioconductor.org/packages/stats/data-annotation/",
        "https://bioconductor.org/packages/stats/data-experiment/",
        "https://bioconductor.org/packages/stats/workflows/"
    ),
    bioc_pkg_types
)

bioc_stat_dumps <- purrr::set_names(
        stringr::str_replace(
            bioc_stat_baseurl,
            "([a-z]+)/$",
            "\\1/\\1_pkg_stats.tab"
        ),
        bioc_pkg_types
    )

bioc_score_dumps <- purrr::set_names(
        stringr::str_replace(
            bioc_stat_dumps,
            "stats\\.",
            "scores\\."
        ),
        bioc_pkg_types
    )
