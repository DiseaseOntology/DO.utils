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
#' - Once per R session (or when `.recheck = TRUE`), execution of ROBOT will be
#' tested. It must succeed prior to any command execution, with success
#' indicated by an announcement of the version & path of ROBOT being used.
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
#' @param .path The path to the ROBOT executable or a robot.jar file.
#'     If `NULL` (default), it will attempt to identify ROBOT on the system path.
#'     See the "ROBOT Setup" section for necessary details. Ignored if a
#'     functional ROBOT executable has been identified during an R session
#'     unless `.recheck = TRUE`.
#' @param .exec Whether to execute the command or not, as boolean (default:
#'     `TRUE`); included primarily for debugging.
#' @param .recheck Whether to re-validate the ROBOT executable, as boolean
#'     (default: `FALSE`).
#'
#' @export
robot <- function(..., .path = NULL, .exec = TRUE, .recheck = FALSE) {
    if (is.null(.path)) {
        robot_cmd <- check_robot(retest = .recheck)
    } else {
        robot_cmd <- check_robot(path = .path, retest = .recheck)
    }
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

    if (!.exec) return(cmd)

    robot_cmd(args = robot_args)
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

# Test if ROBOT is available at path or system (default) & functional.
# On success, cache & return path
check_robot <- function(path = NULL, retest = FALSE) {
    if (retest || is.null(DO_env$robot)) {
        if (is.null(path)) {
            robot_path <- Sys.which("robot")
        } else {
            robot_path <- normalizePath(path, mustWork = TRUE)
        }

        if (tools::file_ext(robot_path) == "jar") {
            cmd <- function(args, ...) {
                system2("java", c("-Xmx10G -jar", robot_path, args), ...)
            }
        } else {
            cmd <- function(args, ...) system2(robot_path, args, ...)
        }

        version <- try(
            cmd("--version", stdout = TRUE, stderr = FALSE),
            silent = TRUE
        )

        if (stringr::str_detect(version, "ROBOT version ")) {
            DO_env$robot_path <- robot_path
            names(DO_env$robot_path) <- version
            DO_env$robot <- cmd
        } else {
            DO_env$robot_path <- NULL
            DO_env$robot <- NULL
        }
    }

    if (is.null(DO_env$robot)) {
        msg <- paste0("ROBOT at ", robot_path, " is not available or working.")
        rlang::abort(msg, class = "robot_fail")
    }

    if (exists("robot_path")) {
        rlang::inform(
            paste0("Using ", names(DO_env$robot_path), " at ", DO_env$robot_path)
        )
    }

    DO_env$robot
}
