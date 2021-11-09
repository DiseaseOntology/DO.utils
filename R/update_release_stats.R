#' Update Release Stats
#'
#' Updates a git repositories release statistics. As this is designed for use
#' with the Human Disease Ontology, the ability to run SPARQL queries for each
#' release is included.
#'
#' @param git_repo A character scalar providing the path to the desired git
#'     repository.
#' @param out_dir A character scalar providing the path to the desired output
#'     directory.
#' @param sparql A character vector listing the path to one or more SPARQL
#'     queries (.rq/.sparql) to be executed for each release and/or to one or
#'     more directories to execute all SPARQL queries they contain (identified
#'     by case-insensitive match to .rq/.sparql file extension while ignoring
#'     subdirectories and the files within them).
#' @param rdf A character vector of the same length as `sparql` listing the
#'     path of rdf files to each query (or directory of queries) should be used
#'     on.
#' @param overwrite A logical scalar indicating whether output files should be
#'     overwritten (default: FALSE).
#'
#' @return
#' The path to the resulting output files, including:
#' * One main release file containing git information about each release,
#' named "{repo_name}-releases.csv"
#' * One file per SPARQL query, named "{repo_name}-releases-{rdf}_{query}.csv"
#' (NOTE: the rdf file's extension is retained, "." replaced by "_", while the
#' query file's extension is dropped).
#'
#' If these files already exist the results will be appended to them unless
#' `overwrite = TRUE`.
#'
#' @export
update_release_stats <- function(git_repo, out_dir, sparql, rdf,
                                 overwrite = FALSE) {

    # validate arguments
    assertthat::assert_that(
        utils::file_test("-d", git_repo),
        utils::file_test("-d", out_dir),
        rlang::is_character(sparql),
        rlang::is_character(rdf),
        length(sparql) == legth(rdf),
        rlang::is_scalar_logical(overwrite)
    )

    # identify sparql file/dir inputs
    file_or_dir <- is_file_or_dir(sparql)
    if (any(!file_or_dir)) {
        rlang::abort(
            message = c("Not a valid file or directory:", sparql[file_or_dir])
        )
    }
    sparql_file <- fs::is_file(sparql)
    sparql_dir <- fs::is_dir(sparql)

    sparql_rdf_df <- purrr::map(
        sparql_dir,
        function(d) {
            list.files(
                path = d,
                pattern = "\\.(rq|sparql)$",
                ignore.case = TRUE,
                full.names = TRUE
            )
        }) %>%
        purrr::set_names(nm = rdf[sparql_dir]) %>%
        tibble::enframe(name = "rdf", value = sparql) %>%
        tidyr::unnest_longer(sparql)

    sparql_rdf_df <- tibble::tibble(
        sparql = sparql[sparql_file],
        rdf = rdf[sparql_file]
    ) %>%
        dplyr::bind_rows(sparql_rdf_df)

    # define output files
    repo_name <- basename(git_repo)
    out_main <- file.path(out_dir, paste0(repo_name, "-releases.csv"))

    sparql_rdf_df <- sparql_rdf_df %>%
        dplyr::mutate(
            rdf_w_ext = stringr::str_replace(
                basename(rdf), "\\.([^.]+)$", "_\\1"
            ),
            sparql_sans_ext = basename_sans_ext(sparql),
            out_path = file.path(
                out_dir,
                glue::glue(
                "{repo_name}-releases-{rdf_w_ext}_{sparql_sans_ext}.csv"
                )
            )
        )

    # release info for main file
    repo <- git2r::repository(git_repo)
    release_df <- git2r::tags(repo) %>%
        purrr::map(as_tibble) %>%
        dplyr::bind_rows(.id = "release") %>%
        dplyr::arrange(when) %>%
        dplyr::mutate(
            date = lubridate::ymd(when),
            release_num = dplyr::row_number()
        ) %>%
        dplyr::select(
            release, release_num, date, sha, author, commit_msg = summary,
            when
        )

    if (file.exists(out_main)) {
        captured_releases <- readr::read_csv(out_main)
        new_releases <- dplyr::anti_join(
            release_df, captured_releases, by = "sha"
        )
    } else {
        new_releases <- release_df
    }
    readr::write_csv(new_releases, out_main, append = TRUE)

    # sparql queries - execute only for new_releases
    sparql_rdf_df <- sparql_rdf_df %>%
        dplyr::mutate(exists = file.exists(out_path))

    # purrr::map(
    #     new_releases$release,
    #     function(.r) {
    #         git2r::checkout(.r)


}


basename_sans_ext <- function(x) {
    x %>%
        tools::file_path_sans_ext() %>%
        basename()
}
