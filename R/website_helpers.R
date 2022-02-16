# plot_def_src() helper
is_nlm_subdomain <- function(subdomain) {
    df <- get(".", env=parent.frame())
    df$Source %in% c("ncbi.nlm.nih.gov", "nlm.nih.gov") &
        stringr::str_detect(df$path, subdomain)
}
