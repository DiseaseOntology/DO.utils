

#' @param release The releases to use for the "before" and "after" trees, as a
#'     named list.
#' @param root The top-level DOID(s) to use for the trees, as a character
#'     vector or named list (see examples). The DOIDs should conform to one
#'     these formats: "DOID:[0-9]+", "DOID_[0-9]+", "obo:DOID_[0-9]+",
#'     "http://purl.obolibrary.org/obo/DOID_[0-9]+".
#'
#' @examples
#' # same root for before & after trees
#' create_before_after_trees(
#'     release = list(
#'         before = "v2022-01-31",
#'         after = "v2022-03-02"
#'     ),
#'     root = "DOID:9351"
#' )
#'
#' # different roots for before & after trees; alternate DOID format
#' create_before_after_trees(
#'     release = list(
#'         before = "v2022-01-31",
#'         after = "v2022-03-02"
#'     ),
#'     root = list(
#'         before = "DOID_9351",
#'         after = "DOID_0081062"
#'     )
#' )
#'
#' # different roots for before & after trees; alternate DOID format
#' create_before_after_trees(
#'     release = list(
#'         before = "v2022-01-31",
#'         after = "v2022-03-02"
#'     ),
#'     root = list(
#'         before = "obo:DOID_9351",
#'         after = "obo:DOID_0081062"
#'     )
#' )
#' @noRd
# create_before_after_trees <- function(DOrepo, release, root,
#                                       save_csv = FALSE) {
#     # validate arguments
#     assertthat::assert_that(
#         is.list(release),
#         length(release) == 2,
#         names(release) == c("before", "after")
#     )
#     if (is.list(root)) {
#         valid_root <- is_valid_doid(unlist(root))
#         assertthat::assert_that(
#             length(unlist(root)) == 2,
#             names(root) == c("before", "after"),
#             all(valid_root)
#         )
#     } else {
#         assertthat::assert_that(
#             rlang::is_string(root),
#             is_valid_doid(root)
#         )
#         root <- list(before = root, after = root)
#         root_URI <- rep(reformat_doid(root), 2)
#     }
#
#     # build tree queries
#     root_URI <- purrr::map_chr(root, reformat_doid, to = "URI")
#     q <- glue::glue('
#         PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#         PREFIX owl: <http://www.w3.org/2002/07/owl#>
#         PREFIX oboInOwl: <http://www.geneontology.org/formats/oboInOwl#>
#
#         SELECT ?id ?label ?parent_id ?parent_label
#         WHERE {
#             ?class a owl:Class ;
#                 rdfs:subClassOf* {root_URI} ;
#                 rdfs:subClassOf ?parent ;
#                 oboInOwl:id ?id ;
#                 rdfs:label ?label .
#             ?parent oboInOwl:id ?parent_id ;
#                 rdfs:label ?parent_label .
#             FILTER(!isblank(?parent))
#         }')
#
#     # Identify releases (aka tags) & setup for checkout
#     repo <- pyDOID$DOrepo(DOrepo)
#     tags <- repo$tags
#     reticulate::iterate(
#         tags,
#         function(t) {
#             if(t$name == release$before) {
#                 release$before <<- t
#             }
#             if(t$name == release$after) {
#                 release$after <<- t
#             }
#         }
#     )
#
#     # execute queries for specified releases & restore repo state
#     res <- query_and_restore(repo, release$before, release$after, q)
#
#     # reformat as cell-based trees
#     purrr::map2(
#         .x = res,
#         .y = root,
#         ~ as_tidygraph(.x, .y, id = "id", parent_id = "parent_id")
#     # optionally, save as csv
#     if (isTRUE(save_csv)) {
#         # default: save to working dir with date + before/after
#         files <- paste0(
#             stringr::str_remove(Sys.Date(), "-"),
#             "-",
#             c("before", "after"),
#             ".csv"
#         )
#         purrr::map2(
#             .x = res,
#             .y = files,
#             ~ readr::write_csv(.x, .y)
#         )
#     }
#
#     if (!is.logical(save_csv)) {
#         if (length(save_csv) != 2) {
#             rlang::warn(
#                 "save_csv should include two file names for before and after",
#                 "warn_save_csv"
#             )
#         } else {
#             purrr::map2(
#                 .x = res,
#                 .y = save_csv,
#                 ~ readr::write_csv(.x, .y)
#             )
#         }
#     }
#
#     res
# }




query_before_after <- function(repo, before_tag, after_tag, query) {
    assertthat::assert_that(length(query) > 0, length(query) <= 2)
    q_before <- query[1]
    if (length(query) > 1) {
        q_after <- query[2]
    } else {
        q_after <- query[1]
    }

    repo$git$checkout(before_tag)
    before_res <- repo$doid$query(query = q_before, load = TRUE) %>%
        tibble::as_tibble()

    repo$git$checkout(after_tag)

    after_res <- repo$doid$query(query = q_after, load = TRUE) %>%
        tibble::as_tibble()

    list(before = before_res, after = after_res)
}

# query_and_restore <- pyDOID$repo$restore_head(query_before_after)


