# only run LOCALLY if ROBOT available (always skips on CI & CRAN)
skip_no_robot <- function(.robot_path = NULL) {
    skip_on_cran()
    skip_on_ci()
    res <- check_robot(.robot_path = .robot_path, on_fail = NULL)
    if (is.null(res)) {
        msg <- "ROBOT is not available"
        if (!is.null(.robot_path)) {
            msg <- paste0(msg, " at ", .robot_path)
        }
        skip(message = msg)
    } else {
        invisible()
    }
}
