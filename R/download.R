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
#' @param ... Additional arguments passed on to [download.file()].
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
    purrr::map2_int(
        .x = url,
        .y = dest_file,
        ~ download.file(.x, .y, ...) %>%
            dl_status$check(.x, .y, abort = on_failure == "abort")
    )

    if (stringr::str_detect(on_failure, "^warn")) {
        dl_status$warn()
    }

    dl_status$return(
        w_failed = stringr::str_detect(on_failure, "list_failed$")
    )
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
       check = function(status, url, dest_file, abort = FALSE) {
           "Check download status of file, with choice to abort on failure."
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
            if (return_failed) {
                list(successful = successful, failed = failed)
            } else {
                successful
            }
        }
    )
)
