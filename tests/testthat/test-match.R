df_all <- DO_pubs %>%
    # ensure priority order here matches that set by pub_id_types & all present
    dplyr::select(dplyr::one_of(pub_id_types)) %>%
    .[1:length(pub_id_types), ]

df_NA <- purrr::map2(
        .x = df_all,
        .y = 1:length(df_all),
        function(col, pos) {
            col[-pos] <- NA
            col
        }
    ) %>%
    dplyr::bind_cols()


test_that("match_citations message is correct", {
    expect_message_for <- function(id_types, drop1 = FALSE) {
        df1 <- dplyr::select(df_all, dplyr::all_of({{ id_types }}))

        if (drop1) {
            id_types <- id_types[-1]
        }
        df2 <- dplyr::select(df_not2, dplyr::all_of({{ id_types }}))

        expect_message(
            match_citations(df1, df2),
            regexp = vctr_to_string(
                pub_id_types[pub_id_types %in% id_types],
                delim = " > "
            )
        )
    }

    # all possible matches in proper order
    expect_message_for(pub_id_types)
    # all possible matches with input in reverse order
    expect_message_for(rev(pub_id_types))
    # single match
    expect_message_for(pub_id_types[1])
    # any 2 at random
    random <- sample(pub_id_types, size = 2)
    expect_message_for(random)
    # 1 less matching column in ref
    expect_message_for(random, drop1 = TRUE)
    # ERRORS
    # trying to match data.frames without same types
    expect_error(match_citations(df_all[1], df_NA[2]))
    # trying to match vectors without same types
    expect_error(match_citations(df_all[[1]], df_NA[[2]]))
})



test_that("match_citations works for data.frames", {
    # all ID types
    expect_equal(match_citations(df_all, df_NA), 1:length(df_all))
    # single ID type
    expect_equal(
        match_citations(df_all[-1], df_NA[-1]),
        c(NA, 2:length(df_all))
    )
    # same column but no matches --> says matching but no result (make warning?)
    df0 <- df_NA[1]
    df0[1] <- NA
    expect_equal(
        match_citations(df_all[1], df0),
        rep(NA_integer_, length(df_all))
    )
})


test_that("match_citations works for vectors", {
    # most important
    expect_equal(
        match_citations(df_all[[1]], df_NA[[1]]),
        c(1, rep(NA, length(df_all) - 1))
    )
    # any other at random
    i <- sample(2:length(df_all), size = 1)
    res <- rep(NA, length(df_all))
    res[i] <- i
    expect_equal(match_citations(df_all[[i]], df_NA[[i]]), res)
})
