# list bioconductor statistics web resources - main site, statistics dump file,
#   download score dump file
#
#   * download score = average downloads over 12 month period
#
# NOTE: stat_dumps & score_dumps are defined as functions to delay run time;
#   use of package `restore_names()` fails on build w/o delay
bioc_stat <- list(
    sites = c(
        software = "http://bioconductor.org/packages/stats/",
        annotation = "http://bioconductor.org/packages/stats/data-annotation.html",
        experiment = "http://bioconductor.org/packages/stats/data-experiment.html",
        workflow = "http://bioconductor.org/packages/stats/workflows.html"
    ),
    stat_dumps = function() {
        stringr::str_replace_all(
            bioc_stat$sites,
            c("stats/$" = "stats/bioc/bioc_pkg_stats.tab",
              "([a-z]+)\\.html" = "\\1/\\1_pkg_stats.tab")
        ) %>%
            # temporary requirement (hopefully)
            restore_names(names(bioc_stat$sites))
    },
    score_dumps = function() {
        stringr::str_replace(
            bioc_stat$stat_dumps(),
            "stats\\.",
            "scores\\."
        ) %>%
            # temporary requirement (hopefully)
            restore_names(names(bioc_stat$sites))
    }
)
