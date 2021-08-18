#' Execute Robot Query
#'
#' Wrapper for system robot program that executes the query subcommand
#'
#' @section NOTE:
#' Requires installation of the OBO Foundry tool
#' [ROBOT](http://robot.obolibrary.org/) in the system path.
#'
#' @section Citation:
#' R.C. Jackson, J.P. Balhoff, E. Douglass, N.L. Harris, C.J. Mungall, and
#' J.A. Overton. ROBOT: A tool for automating ontology workflows.
#' BMC Bioinformatics, vol. 20, July 2019.
#'
#' @param input path to owl file to be queried
#' @param rq file with SPARQL query to execute
#' @param save path to save output file to
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
