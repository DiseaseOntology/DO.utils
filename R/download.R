#' Download Alliance .tsv.gz File
#'
#' Downloads a URL-specified .tsv.gz file from the Alliance of Genome
#' Resources. Files can be found at
#' <https://www.alliancegenome.org/downloads>. Right-click on the "tsv" link
#' of a desired file and select "Copy Link" to get the file URL.
#'
#' A date stamp indicating download date is added to the base file name.
#'
#' @section Recommendation:
#' Although it's possbile to directly read a file from the URL, downloading it
#' promotes reproducibility and ensures future access if needed.
#'
#' @param dest_dir path to directory where file will be saved
#' @param url URL to Alliance file; if not provided, will be requested at console
#' @inheritParams download_file
#'
#' @return
#' Path to saved file.
#'
#' @export
download_alliance_tsv <- function(dest_dir, url = NULL, ...) {
    # Use default URL, if missing
    if (missing(url)) {
        url <- "https://fms.alliancegenome.org/download/DISEASE-ALLIANCE_COMBINED.tsv.gz"
    }

    dest_file <- file.path(dest_dir, basename(url))

    # avoid overwrite if file exists
    if (file.exists(dest_file)) {
        message(dest_file, " exists. Archiving...\n")
        file_version <- alliance_version(dest_file, as_string = TRUE)
        archive_file <- stringr::str_replace(
            dest_file,
            "\\.tsv\\.gz",
            paste0("-", file_version, ".tsv.gz")
        )
        # if archive already exists validate 2 files are identical & delete
        # instead of moving file (or fail if not identical)
        if (file.exists(archive_file)) {
            dest_file_md5 <- tools::md5sum(dest_file)
            archive_md5 <- tools::md5sum(archive_file)

            if (dest_file_md5 == archive_md5) {
                message("Archive file ", archive_file,
                        " already exists.\n Removing ", dest_file, "\n")

                file.remove(dest_file)
            } else {
                stop(
                    paste0("Archive file ", archive_file,
                           " already exists but md5sums differ. Aborting...")
                )
            }
            # if archive does not exist rename file with alliance version & file
            # datetime
        } else {
            message("File archived as ", archive_file, "\n")
            file.rename(dest_file, archive_file)
        }
    }

    # download new file
    download_file(url, dest_file, on_failure = "abort", ...)
}


#' Download OBO Foundry Ontology File
#'
#' Downloads the current version of one or more ontologies from the OBO Foundry.
#'
#' @param ontology_id A character vector of OBO Foundry ontology identifier(s)
#'     (lowercase, as found on http://www.obofoundry.org/). For reference,
#'     ontology identifiers are also provided in [obofoundry_metadata] within
#'     this package.
#' @param dest_dir Path to directory where files will be saved.
#' @inheritParams download_file
#'
#' @inherit download_file return
#'
#' @export
download_obo_ontology <- function(ontology_id, dest_dir, on_failure = "warn",
                                  ...) {

    # validate ontology_id
    oid <- match.arg(
        ontology_id,
        choices = obofoundry_metadata$id,
        several.ok = TRUE
    )
    assertthat::assert_that(
        length(oid) == length(ontology_id),
        msg = paste0(
            "ontology_id(s) do not match OBO Foundry ontology ID(s): ",
            vctr_to_string(ontology_id[!ontology_id %in% oid], delim = ", ")
        )
    )

    # subset to non-obsolete ontologies and set dest_file
    obofoundry_records <- obofoundry_metadata %>%
        dplyr::filter(.data$id %in% oid & !.data$is_obsolete) %>%
        dplyr::mutate(
            dest_file = file.path(dest_dir, basename(.data$ontology_purl))
        )

    # warn about obsolete ontologies, which are not available for download
    obsolete <- oid[!oid %in% obofoundry_records$id]
    if (length(obsolete) > 0) {
        rlang::warn(
            message = c("Obsolete ontologies will be skipped", obsolete)
        )
    }

    # download ontologies
    download_file(
        url = obofoundry_records$ontology_purl,
        dest_file = obofoundry_records$dest_file,
        on_failure = on_failure,
        ...
    )
}


#' Download OMIM Files
#'
#' Downloads one or more specified files from [OMIM](https://www.omim.org/)
#' (the Online catalog of Mendelian Inheritance in Man). `mim2gene.txt` is the
#' only file accessible without an API key from OMIM.
#'
#' @param omim_file A character vector of one or more OMIM file name(s).
#' @param dest_dir Path to directory where files will be saved; all OMIM files
#' are saved in .txt format and consist of tab-separated values with a
#' copyright header.
#' @param api_key Your API key from OMIM, as a string; required if downloading
#' any file other than 'mim2gene.txt'.
#'
#' [Register for downloads](https://www.omim.org/downloads) at OMIM.
#' @inheritParams download_file
#'
#' @inherit download_file return
#'
#' @seealso [read_omim()] to read downloaded files as tibble/data.frames.
#'
#' @export
download_omim <- function(omim_file, dest_dir, api_key = NULL,
                          on_failure = "abort", ...) {
    omim_file <- match.arg(
        omim_file,
        choices = c("mim2gene", "genemap2", "mimTitles", "morbidmap",
                    "phenotypicSeries"),
        several.ok = TRUE
    )
    omim_file <- unique(omim_file)

    # mim2gene is freely accessible to anyone for download, all other files
    # require permission
    if (any(omim_file != "mim2gene") && is.null(api_key)) {
        rlang::abort("An API key is needed for all downloads from OMIM other than mim2gene (https://www.omim.org/downloads).")
    }

    base_url <- "https://data.omim.org/downloads"
    omim_file <- dplyr::if_else(
        stringr::str_detect(omim_file, "\\.txt$"),
        omim_file,
        paste0(omim_file, ".txt")
    )
    url <- dplyr::if_else(
        omim_file == "mim2gene.txt",
        "https://omim.org/static/omim/data/mim2gene.txt",
        paste(base_url, api_key, omim_file, sep = "/")
    )
    dest <- file.path(dest_dir, omim_file)

    download_file(url = url, dest_file = dest, on_failure = on_failure, ...)
}


# General Download --------------------------------------------------------

#' Download File(s) from the Internet
#'
#' `download_file()` is a vectorized version of [utils::download.file()] that
#' does not rely on specifying the "libcurl" method for vectorization.
#'
#' @param url A character vector naming the URL of resource(s) to be downloaded.
#' @param dest_file A character vector with the file path(s) where downloaded
#'     file(s) will be saved.
#' @param on_failure A string indicating how to handle download failure:
#' - "warn" - produce a warning; includes exit codes for debugging
#' - "abort" - abort execution
#' - "list_failed" - list URLs that failed (_output format differs_, see
#'     `Value`)
#' - "warn-list_failed" - combination of "warn" and "list_failed"
#' - "skip" - do nothing
#' @param ... Additional arguments passed on to [utils::download.file()].
#'
#' @return
#' Unless `on_failure` includes "list_failed", the successfully downloaded
#' `dest_file`(s); otherwise, a 2-vector list where `successful` =
#' `dest_file`(s) and `failed` = `url`(s).
#'
#' @export
download_file <- function(url, dest_file, on_failure = "warn", ...) {
    assertthat::assert_that(length(dest_file) == length(url))
    on_failure <- match.arg(
        on_failure,
        choices = c("warn", "abort", "list_failed", "warn-list_failed", "skip")
    )

    dl_status <- download_status$new()
    purrr::map2(
        .x = url,
        .y = dest_file,
        .f = function(.url, .file) {
            utils::download.file(url = .url, destfile = .file, ...) %>%
                dl_status$check(.url, .file, abort = on_failure == "abort")
        }
    )

    if (stringr::str_detect(on_failure, "^warn")) {
        dl_status$warn()
    }

    dl_status$return(
        w_failed = stringr::str_detect(on_failure, "list_failed$")
    )
}


# Import 'new()' method for Ref Classes
#' @importFrom methods new
NULL


#' File Download Status (RefClass)
#'
#' A reference class to track file download status.
#'
#' @field successful A character vector for paths of successfully downloaded
#'      files.
#' @field failed A character vector for paths of files that failed to download
#' @field fail_status An integer vector with an exit code from
#'     [utils::download.file()] for each file in `failed`.
#'
#' @noRd
download_status <- methods::setRefClass(
    "download_status",
    fields = list(
        successful = "character",
        failed = "character",
        fail_status = "numeric"
    ),
    methods = list(
       check = function(status, url, dest_file, abort = FALSE) {
           "Check download status of file with choice to abort on failure."
            if (status == 0) {
                successful <<- c(successful, dest_file)
            } else {
                failed <<- c(failed, url)
                fail_status <<- c(fail_status, status)
            }

            if (abort & length(failed) > 0) {
                if (length(successful) > 0) {
                    # rlang::inform(c("Successfully downloaded:", successful))
                    successful
                }
                rlang::abort(
                    message = c("Download failed (url - exit code):",
                                paste(failed, fail_status, sep = " - ")
                    )
                )
            }
        },
        warn = function() {
            "Warn about failed downloads."
            if (length(failed) > 0) {
                rlang::warn(
                    message = c("Failed to download (file - exit code):",
                                paste(failed, fail_status, sep = " - ")
                    )
                )
            }
        },
        return = function(w_failed = FALSE) {
            "Return successful file paths and, optionally, failed URLs."
            if (w_failed) {
                list(successful = successful, failed = failed)
            } else {
                successful
            }
        }
    )
)
