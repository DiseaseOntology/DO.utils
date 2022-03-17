pyobo_gutils <- NULL
pyDOID <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global references to python modules
    pyobo_gutils <<- reticulate::import("pyobo.gilda_utils", delay_load = TRUE)
    pyDOID <<- reticulate::import("pyDOID", delay_load = TRUE)
}
