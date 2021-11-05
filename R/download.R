#' Check Exit Status of Downloads (Internal)
#'
#' Check the return value when [utils::download.file()] is used, as recommended
#' in the "Good practice" section of it's documentation.
#'
#' @param exit_code An integer vector of exit code(s) produced by
#'     [utils::download.file()].
#' @param dest_file A character vector of relative or absolute paths to
#'     downloaded files. Used only for reporting purposes; the existence of
#'     these files is not checked.
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
