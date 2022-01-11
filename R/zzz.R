pyobo_gutils <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global references to python modules
    pyobo_gutils <<- reticulate::import("pyobo.gilda_utils", delay_load = TRUE)
}
