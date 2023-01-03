test_that("certify_unique_citations() works", {
    # no problems
    citation_df <- tibble::tibble(
        fake1 = 1:4,
        fake2 = letters[1:4],
        pmid = c("36157238", "36104825","36085310", "35967426"),
        pmcid = c("PMC9499748", "PMC9472414", "PMC9463154", "PMC9365668"),
        doi = c("10.1155/2022/2469592", "10.1186/s12957-022-02760-6",
                "10.1038/s41467-022-33026-0", "10.3389/fimmu.2022.942446")
    )

    expect_equal(certify_unique_citations(citation_df), TRUE)


    # 5th row: all IDs = NA
    citation_rowna <- tibble::add_row(
        citation_df,
        fake1 = 5, fake2 = letters[5], doi = NA, pmid = NA, pmcid = NA
    )
    expect_error(
        certify_unique_citations(citation_rowna),
        class = "citation_df-row_na",
        regexp = "5"
    )


    # copy 1st row as 5th
    citation_row_dup <- dplyr::bind_rows(citation_df, citation_df[1, ])
    expect_error(
        certify_unique_citations(citation_row_dup),
        class = "citation_df-row_dup",
        regexp = "1,5"
    )

    # replace 4th pmid & doi with 1st, 2nd pmcid with 1st
    citation_id_dup <- citation_df
    citation_id_dup[4, c("pmid", "doi")] <- citation_id_dup[1, c("pmid", "doi")]
    citation_id_dup[2, "pmcid"] <- citation_id_dup[1, "pmcid"]
    expect_error(
        certify_unique_citations(citation_id_dup),
        class = "citation_df-id_dup",
        # msg truncated = can't include confirmation for doi
        regexp = c("pmid: 1,4.*pmcid: 1,2")
    )
})
