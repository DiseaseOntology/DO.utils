#' Count AGR Entities
#'
#' Counts entities in data from the Alliance of Genome Resources (AGR) by model
#' organism database (MOD) and, optionally, by entity type.
#'
#' @inheritParams assign_entity_to_mod
#' @param by_type logical indicating whether to count by entity type
#' @param pivot logical indicating whether to pivot type to columns; ignored if
#'     type = FALSE
#'
#' @return
#' A summary tibble.
#'
#' @export
count_AGR_entity <- function(AGR_df, by_type = TRUE, pivot = TRUE) {

    mod_assigned_df <- assign_entity_to_mod(AGR_df)

    if (isTRUE(by_type)) {

        mod_count <- mod_assigned_df %>%
            dplyr::mutate(
                type = dplyr::recode(
                    DBobjectType,
                    "affected_genomic_model" = "model"
                ),
                type = factor(type, levels = c("gene", "allele", "model"))
            ) %>%
            dplyr::count(
                mod_assigned, type,
                name = "entity_n"
            )

        if (isTRUE(pivot)) {
            mod_count <- mod_count %>%
                tidyr::pivot_wider(
                    names_from = type,
                    values_from = entity_n
                ) %>%
                # dplyr::select(mod_assigned, gene, allele, model) %>%
                dplyr::rename_with(.fn = ~paste0(.x, "_n"), .cols = -mod_assigned)
        }

    } else {

        mod_count <- dplyr::count(
            mod_assigned_df, mod_assigned,
            name = "entity_n"
        )

    }

    mod_count
}

#' Assign AGR Entities to MODs
#'
#' Assigns Alliance of Genome Resource (AGR) entities to the appropriate model
#' organism database (MOD) using a step-wise approach. AGR entities may be
#' "sourced" from the "Alliance" obscuring which MOD the annotations were
#' originally made in.
#'
#' The step-wise approach used to assign entities is as follows:
#'
#' 1. `Source` is used where it pertains to a MOD (and not the "Alliance");
#' **Note:** "OMIM Via RGD" is assigned to RGD
#'
#' 2. `DBObjectID` is used where it comes from a MOD (i.e. not "HGNC")
#'
#' 3. `WithOrthologs` is used where the ID comes from a MOD
#'
#' 4. Any remaining entities are left assigned to the "Alliance" because their
#' original source can not be identified
#'
#' @return
#' The input dataframe with an additional column (`mod_assigned`) indicating the
#' assignment.
#'
#' @param AGR_df a dataframe derived from AGR data (usually a
#' [downloaded .tsv file](https://www.alliancegenome.org/downloads))
assign_entity_to_mod <- function(AGR_df) {
    df_out <- dplyr::mutate(
        AGR_df,
        namespace_id = stringr::str_remove(DBObjectID, ":.*"),
        ortholog_namespace_id = stringr::str_remove(WithOrthologs, ":.*"),
        mod = dplyr::case_when(
            Source != "Alliance" ~ Source,
            namespace_id != "HGNC" ~ namespace_id,
            !is.na(ortholog_namespace_id) ~ ortholog_namespace_id,
            TRUE ~ Source
        ),
        mod_assigned = id_mod(mod)
    )

    df_out <- dplyr::select(
        df_out,
        -namespace_id, -ortholog_namespace_id, -mod
    )

    df_out
}

#' Identifies Alliance MOD
#'
#' Replace Alliance MOD codes with recognizable abbreviation or name used by
#' the Alliance.
#'
#' @param x character vector of Alliance "Source" codes
#'
#' @noRd
id_mod <- function(x) {
    mod_codes <- c(
        FB = "FlyBase", MGI = "MGD", RGD = "RGD", `OMIM Via RGD` = "RGD",
        SGD = "SGD", WB = "WormBase", ZFIN = "ZFIN" #, Alliance = "Alliance"
    )

    dplyr::recode(x, !!!mod_codes)
}

#' Download and Read AGR .tsv.gz files
#'
#' Downloads a URL-specified .tsv.gz file from the Alliance of Genome
#' Resources (AGR) and reads that file into R as a tibble. Files can be found at
#' <https://www.alliancegenome.org/downloads>. Right-click on the "tsv" link
#' of a desired file and select "Copy Link" to get the file URL.
#'
#' The file is downloaded for the purpose of reproducibility and future
#' reference. A date stamp will be added to the base file name.
#'
#' @param url URL to AGR file
#' @param dest_dir path to directory where file will be saved
#'
#' @return
#' A dataframe.
#'
#' @export
dl_read_AGR <- function(url, dest_dir) {

    # build destination file path from URL
    date_stamp <- format(Sys.time(), "%Y%m%d")
    filename <- stringr::str_replace(
        basename(url),
        "(^.*)\\.tsv\\.gz",
        paste0("\\1-", date_stamp, ".tsv.gz")
    )
    dest_file <- file.path(dest_dir, filename)

    # download
    resp <- utils::download.file(url, dest_file)

    assertthat::assert_that(
        resp == 0,
        msg = paste0("Download failed with exit code: ", resp)
    )

    # read
    ## identify header (to skip)
    ##  Skipping instead of using comment = "#" because data gets truncated
    ##  where values contain "#" (e.g. 'Tg(Alb-Mut)#Cpv')
    header_end <- readr::read_lines(dest_file, n_max = 100) %>%
        stringr::str_detect("^#") %>%
        which() %>%
        max()

    AGR_df <- readr::read_tsv(
        dest_file,
        skip = header_end,
        col_types = readr::cols(.default = readr::col_character())
    )

    AGR_df
}
