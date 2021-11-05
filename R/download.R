#' Download File(s) from the Internet
#'
#' `download_file()` is a vectorized version of [utils::download.file()] that
#' does not rely on specifying the "libcurl" method for vectorization.
#'
#' @param url A character vector naming the URL of resource(s) to be downloaded.
#' @param dest_file A character vector with the file path(s) where downloaded
#'     file(s) will be saved.
#' @param on_failure A string indicating how to handle download failure.
#' @param ... Additional arguments passed on to [download.file()].
#'
#' @return
#' A character vector of paths to destination files that downloaded
#' successfully along with a warning indicating which failed when
#' `abort = FALSE` (default).
#'
#' @export
download_file <- function(url, dest_file, on_failure = "warn", ...) {
    assertthat::assert_that(length(dest_file) == length(url))
    on_failure <- match.arg(
        on_failure,
        choices = c("warn", "abort", "list_failed", "warn-list_failed", "skip"))

    dl_status <- download_status$new()
    purrr::map2_int(
        .x = url,
        .y = dest_file,
        ~ dl_status$check(
            download.file(.x, .y, ...),
            .x,
            .y,
            abort = on_failure == "abort"
        )
    )

    if (stringr::str_detect(on_failure, "^warn")) {
        dl_status$warn()
    }

    dl_status$return(
        w_failed = stringr::str_detect(on_failure, "list_failed$")
    )
}


#' Check Exit Status of Downloads (Internal)
#'
#' Check the return value when [utils::download.file()] is used, as recommended
#' in the "Good practice" section of it's documentation.
#'
#' @param exit_code An integer vector of exit code(s) produced by
#'     [utils::download.file()].
#' @param dest_file A character vector of relative or absolute paths to
#'     downloaded files. Used to make output and messages more informative; the
#'     existence of these files is not tested.
#' @param abort A logical scalar indicating whether to abort when non-zero
#'     exit codes are produced. When `FALSE`, (default) failed downloads will
#'     produce warnings. `TRUE` will likely only be useful for single file
#'     downloads.
#'
#' @return
#' A character vector of paths to destination files that downloaded
#' successfully, with a warning indicating which fail when `abort = FALSE`
#' (default).
check_dl_status <- function(exit_code, dest_file, abort = FALSE) {
    assert_scalar_logical(abort)
    failed <- exit_code != 0
    if (any(failed)) {
        msg = c("File(s) failed to download",
                paste0(dest_file[failed], " - exit code ", exit_code[failed])
        )
        if (abort) {
            rlang::inform(
                c("File(s) downloaded successfully:", dest_file[!failed])
            )
            rlang::abort(message = msg)
        }
        rlang::warn(message = msg)
    }
    dest_file[!failed]
}


#' A Reference Class to represent file download status.
#'
#' @field successful A character vector for paths of successfully downloaded
#'      files.
#' @field failed A character vector for paths of files that failed to download
#' @field fail_status An integer vector with an exit code from [download.file()]
#'     for each file in `failed`.
download_status <- setRefClass(
    "download_status",
    fields = list(
        successful = "character",
        failed = "character",
        fail_status = "numeric"
    ),
    methods = list(
       check = function(status, url, destfile, abort = FALSE) {
           "Check download status of file, with choice to abort on failure."
            if (status == 0) {
                successful <<- c(successful, destfile)
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
            if (return_failed) {
                list(successful = successful, failed = failed)
            } else {
                successful
            }
        }
    )
)
