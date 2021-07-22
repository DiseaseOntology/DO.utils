# DO R packages
DO_dep_pkg <- list(software = "DOSE", annotation = "DO.db")

bioc_pkg_types <- c("software", "annotation", "experiment", "workflow")

# list bioconductor statistics web resources - main site, base urls,
#   statistics dump file, download score dump file
#
#   * download score = average downloads over 12 month period
bioc_stat_sites <- purrr::set_names(
        c(
            "http://bioconductor.org/packages/stats/",
            "http://bioconductor.org/packages/stats/data-annotation.html",
            "http://bioconductor.org/packages/stats/data-experiment.html",
            "http://bioconductor.org/packages/stats/workflows.html"
        ),
        bioc_pkg_types
    )

bioc_stat_baseurl <- purrr::set_names(
    c(
        "http://bioconductor.org/packages/stats/bioc/",
        "http://bioconductor.org/packages/stats/data-annotation/",
        "http://bioconductor.org/packages/stats/data-experiment/",
        "http://bioconductor.org/packages/stats/workflows/"
    ),
    bioc_pkg_types
)

bioc_stat_dumps <- purrr::set_names(
        stringr::str_replace_all(
            bioc_stat_baseurl,
            c("stats/$" = "stats/bioc/bioc_pkg_stats.tab",
              "([a-z]+)\\.html" = "\\1/\\1_pkg_stats.tab")
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
