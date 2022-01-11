pyobo_gutils <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global reference to pyobo_gutils
    pyobo_gutils <<- reticulate::import("pyobo.gilda_utils", delay_load = TRUE)
}
