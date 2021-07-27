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

