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
#' - Can also use a standalone robot.jar file (`.robot_path` must be specified)
#' in which case it is executed with `java -Xmx10G -jar` (10 GB memory).
#' - On the first use of each R session, the ROBOT executable/.jar specified
#' will be tested. It must succeed prior to any command execution. On success,
#' `robot()` will announce the version & path being used. If at any time a new
#' `.robot_path` is specified, this previous version will be replaced with a new
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
#' @param .robot_path The path to a ROBOT executable or .jar file, as a string.
#'     When `NULL` (default), if a system ROBOT executable is available it will
#'     be used, otherwise an error will be signaled.
#'
#' **NOTE:** `DO.utils` caches the last ROBOT used for future use.
#'
#' @export
robot <- function(..., .robot_path = NULL) {
    check_robot(.robot_path)
    dots <- convert_robot_tf(...)
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

    status <- suppressWarnings(DO_env$robot(args = robot_args))
    status <- check_robot_error(status)
    if (length(status) > 0) rlang::inform(status)
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
            c(
                `1` = "download robot", `2` = "chmod robot",
                `3` = "download robot.jar"
            )
        )

        warning(
            "Non-zero exit code(s):\n",
            paste0(non_zero_fxn, ": ", exit[non_zero_codes], "\n")
        )
    }
}


# robot() helpers ---------------------------------------------------------

#' Check for ROBOT
#'
#' Check if ROBOT is functional and cache it for future use.
#'
#' @inheritParams robot
#' @param on_fail What to do on fail: "error" (default), "warn", "inform", or
#' `NULL` (meaning do nothing).
#'
#' @returns Path and version of functional ROBOT. Use [robot()] for execution.
#'
#' @noRd
check_robot <- function(.robot_path = NULL, on_fail = "error") {
    if (is.null(.robot_path) && !is.null(DO_env$robot)) {
        return(DO_env$robot_path)
    }
    if (is.null(.robot_path)) {
        DO_env$robot_path <- Sys.which("robot")
    } else {
        .robot_path <- suppressWarnings(normalizePath(.robot_path))
        if (!is.null(DO_env$robot_path) && .robot_path == DO_env$robot_path) {
            rlang::inform("ROBOT path unchanged.")
            return(DO_env$robot_path)
        } else {
            DO_env$robot_path <- .robot_path
        }
    }

    if (tools::file_ext(DO_env$robot_path) == "jar") {
        DO_env$robot <- function(args, ...) {
            system2(
                "java", c("-jar", DO_env$robot_path, args),
                stdout = TRUE, stderr = TRUE, ...
            )
        }
    } else {
        DO_env$robot <- function(args, ...) {
            system2(
                DO_env$robot_path, args,
                stdout = TRUE, stderr = TRUE, ...
            )
        }
    }

    version <- try(DO_env$robot("--version"), silent = TRUE)

    if (stringr::str_detect(version, "ROBOT version ")) {
        names(DO_env$robot_path) <- version
        rlang::inform(
            paste0(version, " at ", DO_env$robot_path, " activated for session.")
        )
    } else {
        DO_env$robot <- NULL
        DO_env$robot_path <- NULL
    }

    if (is.null(DO_env$robot) && !is.null(on_fail)) {
        if (is.null(.robot_path)) {
            msg <- "ROBOT cannot be found. Please specify a path to robot.jar, or install and add it to the system path (see https://robot.obolibrary.org)"
        } else {
            msg <- paste0("ROBOT at ", .robot_path, " is not available or working.")
        }

        msg_fxn <- switch(
            on_fail,
            error = rlang::abort,
            warn = rlang::warn,
            inform = rlang::inform
        )
        msg_fxn(msg, class = "robot_fail")
    }

    invisible(DO_env$robot_path)
}

# check if status is indicative of error
check_robot_error <- function(status) {
    if (length(status) == 1 && is.numeric(status)) {
        if (status == 0) {
            return()
        }
    } else {
        exit_status <- attr(status, "status")
        if (is.null(exit_status) || exit_status == 0) {
            return(status)
        }
    }

    java_error <- any(
        stringr::str_detect(status, stringr::coll("java", ignore_case = TRUE))
    )
    if (java_error) robot_error(status, "java_error")
    robot_error(status)
}

# signal errors during execution of ROBOT program
robot_error <- function(msg = NULL, class = NULL) {
    if (is.null(msg)) {
        msg <- paste0("Requested ROBOT program is not available or working.")
    } else {
        msg <- c(msg[1], vctr_to_string(msg[-1], delim = "\n  "))
    }

    class <- append(class, values = "robot_error")
    rlang::abort(msg, class, .frame = parent.frame())
}

# convert TRUE/FALSE to robot-compatible "true"/"false"
convert_robot_tf <- function(...) {
    x <- list(...)
    purrr::map(
        x,
        function(.x) {
            if (!.x %in% c(TRUE, FALSE) & !.x %in% c("TRUE", "FALSE")) {
                return(.x)
            }
            tolower(.x)
        }
    )
}
