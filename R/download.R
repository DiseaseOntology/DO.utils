#' Download File(s) from the Internet
#'
#' `download_file()` is a vectorized version of [utils::download.file()] that
#' does not rely on specifying the "libcurl" method for vectorization.
#'
#' @param url A character vector naming the URL of resource(s) to be downloaded.
#' @param dest_file A character vector with the file path(s) where downloaded
#'     file(s) will be saved.
#' @inheritParams check_dl_status
#' @param ... Additional arguments passed on to [download.file()].
#'
#' @return
#' A character vector of paths to destination files that downloaded
#' successfully along with a warning indicating which failed when
#' `abort = FALSE` (default).
#'
#' @export
download_file <- function(url, dest_file, abort = FALSE, ...) {
    assertthat::assert_that(length(dest_file) == length(url))

    exit_code <- purrr::map2_int(
        .x = url,
        .y = dest_file,
        ~ download.file(.x, .y, ...)
    )

    check_dl_status(exit_code, dest_file, abort = abort)
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
