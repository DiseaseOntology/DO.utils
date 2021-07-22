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
