pyobo_gutils <- NULL
py_rdf <- NULL
pyDOID <- NULL

.onLoad <- function(libname, pkgname) {
    # use superassignment to update global references to python modules
    pyobo_gutils <<- reticulate::import("pyobo.gilda_utils", delay_load = TRUE)
    pyDOID <<- reticulate::import("pyDOID", delay_load = TRUE)
    py_rdf <<- reticulate::import_from_path(
        "py_rdf",
        system.file(
            "python",
            package = utils::packageName(),
            mustWork = TRUE
        ),
        convert = TRUE,
        delay_load = TRUE
    )
}
