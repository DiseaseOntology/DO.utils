#' Execute Robot Commands
#'
#' Light wrapper for OBO Foundry ROBOT program (OBO Foundry). See
#' [ROBOT documentation](http://robot.obolibrary.org/) for information about
#' subcommands and arguments.
#'
#' @section ROBOT Setup:
#' - Requires installation of Java (supported versions are listed under the
#' "Getting Started" section at http://robot.obolibrary.org/).
#' - Can use OBO Foundry ROBOT tool installed on the system path. This
#' may be achieved with [install_robot()] or as described at
#' http://robot.obolibrary.org/.
#' - Can also use a standalone robot.jar file (`.path` must be specified) in
#' which case it is executed with `java -Xmx10G -jar` (10 GB memory).
#' - On the first use of each R session, the ROBOT executable/.jar specified
#' will be tested. It must succeed prior to any command execution. On success,
#' `robot()` will announce the version & path being used. If at any time a new
#' `.path` is specified, this previous version will be replaced with a new
#' announcement.
#'
#' @section Citation:
#' R.C. Jackson, J.P. Balhoff, E. Douglass, N.L. Harris, C.J. Mungall, and
#' J.A. Overton. ROBOT: A tool for automating ontology workflows.
#' BMC Bioinformatics, vol. 20, July 2019.
#'
#' @param ... Command(s) passed on to ROBOT, including subcommand(s), either as
#'     a single string or named strings, where the name corresponds to the long
#'     or short form of the option and the value being the option value. Where
#'     options do not use a value, use "". Subcommands should be unnamed.
#'
#' **Examples:**
#'
#' * Subcommands: `"query"`, `"export"`
#' * Common options:
#'     * `"--input doid.owl"` or `input = "doid.owl"` (named argument).
#'     * `"-o result.owl"` (i.e. `--output`) or `o = "result.owl"` (named argument).
#'     * `"--remove-annotations"` (option of `annotate`) or
#'         `"remove-annotations" = ""` (named argument form).
#'
#' @param .path The path to a ROBOT executable or .jar file, as a string.
#'     If `NULL` (default), the system path will be searched for the ROBOT
#'     executable. See the "ROBOT Setup" section for installation details.
#'
#' @export
robot <- function(..., .path = NULL) {
    check_robot(.path)
    dots <- list(...)
    args <- stringr::str_trim(names(dots))

    if (length(args) == 0) {
        robot_args <- unlist(dots)
    } else {
        args <- dplyr::case_when(
            stringr::str_length(args) < 1 ~ args,
            stringr::str_length(args) == 1 ~ paste0("-", args),
            stringr::str_length(args) > 1 ~ paste0("--", args)
        )
        robot_args <- purrr::map2_chr(
            args,
            unlist(dots, use.names = FALSE),
            ~ paste(.x, .y)
        )
    }

    DO_env$robot(args = robot_args)
}


#' Execute Robot Query
#'
#' Wrapper for system ROBOT program (OBO Foundry) that executes the query
#' subcommand. See [ROBOT documentation](http://robot.obolibrary.org/) for
#' details.
#'
#' @section NOTE:
#' Requires installation of the OBO Foundry ROBOT tool in the system path. This
#' can be achieved with [install_robot()].
#'
#' @section Citation:
#' R.C. Jackson, J.P. Balhoff, E. Douglass, N.L. Harris, C.J. Mungall, and
#' J.A. Overton. ROBOT: A tool for automating ontology workflows.
#' BMC Bioinformatics, vol. 20, July 2019.
#'
#' @param input path to owl file to be queried
#' @param rq file with SPARQL query to execute
#' @param save path to save result to
#'
#' @export
robot_query <- function(input, rq, save) {
    system2(
        # use system robot command
        "robot",
        # args - subcommand, input, query (rq & save)
        c("query",
          paste0("--input ", input),
          paste(
              "--query",
              rq,
              save,
              sep = " "
          )
        )
    )
}


#' Install OBO Foundry ROBOT Tool (Mac/Linux ONLY)
#'
#' Installs latest ROBOT and robot.jar files to default system path
#' (/usr/local/bin). Only tested for Macs but also will likely work for Linux.
#'
#' @param ... arguments passed on to [utils::download.file()]; should not be
#' needed
#'
#' @return
#' The version of robot.jar, if successful, otherwise a warning indicating where
#' failures occurred along with their non-zero integer codes. See documentation
#' of [utils::download.file()] (for download of robot/robot.jar) and [system2()]
#' (for making robot executable; uses system `chmod`) under "Value" for more
#' details.
#'
#' @export
install_robot <- function(...) {

    robot_file <- "/usr/local/bin/robot"
    jar_file <- "/usr/local/bin/robot.jar"

    exit <- integer()

    # get robot batch file
    exit[1] <- utils::download.file(
        url = "https://raw.githubusercontent.com/ontodev/robot/master/bin/robot",
        destfile = robot_file,
        ...
    )

    # make robot batch file executable by USER only
    exit[2] <- as.integer(
        system2(
            "chmod",
            args = c("u+x", robot_file, "; echo $?"),
            stdout = TRUE
        )
    )

    # get latest robot.jar file
    exit[3] <- utils::download.file(
        url = "https://github.com/ontodev/robot/releases/latest/download/robot.jar",
        destfile = jar_file,
        ...
    )

    if (all(exit == 0)) {
        invisible(check_robot())
    } else {
        non_zero_codes <- exit != 0
        non_zero_fxn <- dplyr::recode(
            which(exit != 0),
            c(`1` = "download robot", `2` = "chmod robot",
              `3` = "download robot.jar")
        )

        warning(
            "Non-zero exit code(s):\n",
            paste0(non_zero_fxn, ": ", exit[non_zero_codes], "\n")
        )
    }
}

#' Check for ROBOT
#'
#' Check if ROBOT is functional and cache it for future use.
#'
#' @inheritParams robot
#'
#' @returns Path and version of functional ROBOT. Use [robot()] for execution.
#'
#' @noRd
check_robot <- function(.path = NULL) {
    if (is.null(.path) && !is.null(DO_env$robot)) return(DO_env$robot_path)
    if (is.null(.path)) {
        DO_env$robot_path <- Sys.which("robot")
    } else {
        .path <- normalizePath(.path, mustWork = TRUE)
        if (!is.null(DO_env$robot_path) && .path == DO_env$robot_path) {
            rlang::inform("ROBOT path unchanged.")
            return(DO_env$robot_path)
        } else {
            DO_env$robot_path <- .path
        }
    }

    if (tools::file_ext(DO_env$robot_path) == "jar") {
        DO_env$robot <- function(args, ...) {
            system2("java", c("-jar", DO_env$robot_path, args), ...)
        }
    } else {
        DO_env$robot <- function(args, ...) system2(DO_env$robot_path, args, ...)
    }

    version <- try(
        DO_env$robot("--version", stdout = TRUE, stderr = FALSE),
        silent = TRUE
    )

    if (stringr::str_detect(version, "ROBOT version ")) {
        names(DO_env$robot_path) <- version
    } else {
        DO_env$robot <- NULL
        DO_env$robot_path <- NULL
    }

    if (is.null(DO_env$robot)) {
        msg <- paste0("ROBOT at ", .path, " is not available or working.")
        rlang::abort(msg, class = "robot_fail")
    }

    rlang::inform(
        paste0("Using ", version, " at ", DO_env$robot_path)
    )

    invisible(DO_env$robot_path)
}
