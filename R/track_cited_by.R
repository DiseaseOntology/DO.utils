
get_citations_main_dribble <- function() {
    dplyr::filter(do_citation_files, name == "DO_citations_main")
}

get_cited_by_dribble <- function() {
    dplyr::filter(
        do_citation_files,
        stringr::str_detect(name, "cited_by")
    )
}

load_DO_pub_list <- function() {
    googlesheets4::read_sheet(do_cited_by_gs, sheet = "DO_pubs")
}
