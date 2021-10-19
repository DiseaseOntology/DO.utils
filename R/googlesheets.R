#' Read Stored Cited By Info
#'
#' Read cited by information stored as Google Sheets.
#'
#' The datasets currently available are:
#'
#' * pubmed: Detailed DO publication cited by list from PubMed (obtained via
#' Entrez API)
#'
#' * scopus: Detailed DO publication cited by list from Scopus (obtained via
#' Scopus Search API)
#'
#' * published_uses: Human-readable summary of merged DO publication cited by
#' lists, along with any curated use information
#'
#' @param dataset A string representing one of the datasets stored in Google
#' Sheets
#'
#' @export
read_citedby <- function(dataset) {

    if (db == "published_uses") {
        spreadsheet <- gs_DO_uses
        sheet <- "published_uses"
    } else {
        spreadsheet <- gs_DO_citedby
        sheet <- db
    }

    googlesheets4::read_sheet(
        ss = spreadsheet,
        sheet = sheet
    )
}
