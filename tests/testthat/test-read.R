test_that("read_omim() works for OFFICIAL download of SEARCH results", {
    expected <- structure(
        list(
            omim = c(
                "OMIM:620058", "OMIM:103850", "OMIM:615018", "OMIM:137100",
                "OMIM:152200", "OMIM:107670", "OMIM:609529", "OMIM:620378",
                "OMIM:137050", "OMIM:131330", "OMIM:168820", "OMIM:611003",
                "OMIM:107410", "OMIM:240150"
            ),
            mim_symbol = c(
                "#", "*", "#", "%", "*", "+", "#", "%", NA, "*", "+", "%", NA, NA
            ),
            mim_type = c(
                "phenotype", "gene", "phenotype",
                "phenotype, unknown molecular basis", "gene",
                "gene, includes phenotype", "phenotype",
                "phenotype, unknown molecular basis", "phenotype, suspected/overlap",
                "gene", "gene, includes phenotype",
                "phenotype, unknown molecular basis", "phenotype, suspected/overlap",
                "phenotype, suspected/overlap"
            ),
            mim_number = c(
                "620058", "103850", "615018", "137100", "152200", "107670", "609529",
                "620378", "137050", "131330", "168820", "611003", "107410", "240150"
            ),
            title = c(
                "FAMILIAL APOLIPOPROTEIN GENE CLUSTER DELETION SYNDROME",
                "ALDOLASE A, FRUCTOSE-BISPHOSPHATE; ALDOA", "BLOOD GROUP, SID SYSTEM; SID",
                "IMMUNOGLOBULIN A DEFICIENCY 1; IGAD1", "LIPOPROTEIN(a); LPA",
                "APOLIPOPROTEIN A-II; APOA2", "IMMUNOGLOBULIN A DEFICIENCY 2; IGAD2",
                "CHARCOT-MARIE-TOOTH DISEASE, DOMINANT INTERMEDIATE A; CMTDIA",
                "GAMMA-A-GLOBULIN, DEFECT IN ASSEMBLY OF", "PROENKEPHALIN; PENK",
                "PARAOXONASE 1; PON1", "SMOKING AS A QUANTITATIVE TRAIT LOCUS 1; SQTL1",
                "SERPIN PEPTIDASE INHIBITOR, CLADE A, MEMBER 2, PSEUDOGENE; SERPINA2P",
                "HYPERVITAMINOSIS A, SUSCEPTIBILITY TO"
            ),
            included_titles = c(
                NA, NA,
                "Sd(a) POLYAGGLUTINATION SYNDROME, INCLUDED; SDPS, INCLUDED",
                NA, "APOLIPOPROTEIN(a), INCLUDED; Apo(a), INCLUDED",
                "APOLIPOPROTEIN A-II DEFICIENCY, INCLUDED", NA, NA, NA, NA,
                "PON1 ENZYME ACTIVITY, VARIATION IN, INCLUDED", NA, NA, NA
            ),
            cytogenetic_location = c(
                "11q23", "16p11.2", "17q21.32",
                "6p21.3", "6q25.3-q26", "1q23.3", "17p11.2", "10q24.1-q25.1",
                NA, "8q12.1", "7q21.3", "10q22", "14q32.1", NA
            ),
            genomic_coordinates_from_ncbi_grch38 = c(
                "11:110600001-121300000", "16:30064279-30070420", NA,
                "6:30500001-36600000", "6:160531482-160664275",
                "1:161222292-161223628", NA, "10:95300001-110100000", NA,
                "8:56440957-56446641", "7:95297676-95324532", "10:68800001-80300000",
                "14:89300001-95800000", NA
            ),
            entrez_gene_id = c(
                NA, "226", NA, "10986", "4018", "336", NA, "130068899", NA, "5179",
                "5444", "100188827", "390502", NA
            )
        ),
        row.names = c(NA, -14L),
        class = c("omim_search", "omim_tbl", "tbl_df", "tbl", "data.frame"),
        omim_official = TRUE
    )

    expect_equal(read_omim("data/omim/omim-search_dl.tsv"), expected)
})

test_that("read_omim() works for OFFICIAL download of PHENOTYPIC SERIES TITLES", {
    expected <- structure(
        list(
            omim = c(
                "OMIM:PS605552", "OMIM:PS200600", "OMIM:PS142690", "OMIM:PS101800"
            ),
            phenotypic_series_title = c(
                "Abdominal obesity-metabolic syndrome", "Achondrogenesis",
                "Acne inversa", "Acrodysostosis"
            ),
            phenotypic_series_number = c(
                "PS605552", "PS200600", "PS142690", "PS101800"
            )
        ),
        row.names = c(NA, -4L),
        class = c("omim_PS_titles", "omim_tbl", "tbl_df", "tbl", "data.frame"),
        omim_official = TRUE
    )

    expect_equal(read_omim("data/omim/omim-ps_titles_dl.tsv"), expected)
})

ps_df <- structure(
    list(
        omim = c("OMIM:PS613135", "OMIM:619738", "OMIM:613135", "OMIM:618049"),
        location = c(NA, "1p12", "5p15.33", "10q25.3"),
        phenotype = c(
            "Parkinsonism-dystonia, infantile",
            "Parkinsonism-dystonia 3, childhood-onset",
            "Parkinsonism-dystonia, infantile, 1",
            "?Parkinsonism-dystonia, infantile, 2"
        ),
        phenotype_mim_number = c("PS613135", "619738", "613135", "618049"),
        inheritance = c(NA, "AR", "AR", "AR"),
        phenotype_mapping_key = c(NA, "3", "3", "3"),
        gene_locus = c(
            NA, "WARS2, NEMMLAS, PKDYS3", "SLC6A3, DAT1, PKDYS1",
            "SLC18A2, VAT2, SVMT, PKDYS2"
        ),
        gene_locus_mim_number = c(NA, "604733", "126455", "193001"),
        geno_inheritance = c(NA, rep("autosomal recessive inheritance", 3))
    ),
    row.names = c(NA, -4L),
    class = c("omim_PS", "omim_tbl", "tbl_df", "tbl", "data.frame"),
    omim_official = TRUE
)

test_that("read_omim() works for OFFICIAL download of PHENOTYPIC SERIES", {
  expect_equal(read_omim("data/omim/omim-ps_dl.tsv"), ps_df)
})

test_that("read_omim() works for COPIED data (PS or with entry info)", {
    ps_df_cp <- ps_df %>%
        dplyr::filter(!stringr::str_detect(.data$omim, "PS")) %>%
        dplyr::mutate(gene_locus = stringr::str_remove(.data$gene_locus, ",.*"))
    attr(ps_df_cp, "omim_official") <- FALSE
    class(ps_df_cp) <- class(ps_df)[-1]

    expect_equal(read_omim("data/omim/omim-ps_cp-ps_page.csv"), ps_df_cp)
    # expect_snapshot(read_omim("data/omim/omim-ps_cp-ps_page_w_ps.csv")) # not supported
    expect_equal(read_omim("data/omim/omim-ps_cp-entry_page.csv"), ps_df_cp)
    # expect_snapshot(read_omim("data/omim/omim-ps_cp-entry_page_w_ps.csv")) # not supported
})
