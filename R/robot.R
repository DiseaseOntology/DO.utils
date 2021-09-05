#' Execute Robot Commands
#'
#' Light wrapper for system ROBOT program (OBO Foundry). See
#' [ROBOT documentation](http://robot.obolibrary.org/) for information about
#' subcommands and arguments.
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
#' @param params string specifying subcommand and all arguments
#' @param ... arguments passed on to [base::system2]
#'
#' @export
robot <- function(params, ...) {
    system2(
        # use system robot command
        command = "robot",
        args = params
    )
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
        system2("robot", args = "--version")
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
