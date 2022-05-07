# match_citations() -------------------------------------------------------

##### DATA #####
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


##### CUSTOM TESTSING FUNCTIONS #####
expect_match_snapshot <- function(id_types, drop1 = FALSE) {
    df1 <- dplyr::select(df_all, dplyr::all_of({{ id_types }}))

    if (drop1) {
        id_types <- id_types[-1]
    }
    df2 <- dplyr::select(df_NA, dplyr::all_of({{ id_types }}))

    suppressMessages(expect_snapshot(match_citations(df1, df2)))
}

expect_eq_no_msg <- function(...) {
    suppressMessages(expect_equal(...))
}


##### TESTS #####
test_that("match_citations message is correct for all possible matches in proper order", {
    expect_match_snapshot(pub_id_types)
})

test_that("match_citations message is correct for all possible matches in reverse order", {
    expect_match_snapshot(rev(pub_id_types))
})

test_that("match_citations message is correct for the highest priority single match", {
    expect_match_snapshot(pub_id_types[1])
})

test_that("match_citations message is correct when 1st match is dropped", {
    expect_match_snapshot(pub_id_types[1:2], drop1 = TRUE)
})

test_that("match_citations errors when expected", {
    # ERRORS
    # trying to match data.frames without same types
    expect_error(match_citations(df_all[1], df_NA[2]))
    # trying to match vectors without same types
    expect_error(match_citations(df_all[[1]], df_NA[[2]]))
})


test_that("match_citations works for data.frames", {
    # all ID types
    expect_eq_no_msg(match_citations(df_all, df_NA), 1:length(df_all))
    # single ID type
    expect_eq_no_msg(
        match_citations(df_all[-1], df_NA[-1]),
        c(NA, 2:length(df_all))
    )
    # same column but no matches --> says matching but no result (make warning?)
    df0 <- df_NA[1]
    df0[1] <- NA
    expect_eq_no_msg(
        match_citations(df_all[1], df0),
        rep(NA_integer_, length(df_all))
    )
})


test_that("match_citations works for vectors", {
    # most important
    expect_eq_no_msg(
        match_citations(df_all[[1]], df_NA[[1]]),
        c(1, rep(NA, length(df_all) - 1))
    )
    # any other at random
    i <- sample(2:length(df_all), size = 1)
    res <- rep(NA, length(df_all))
    res[i] <- i
    expect_eq_no_msg(match_citations(df_all[[i]], df_NA[[i]]), res)
})


test_that("match_citations add_col works", {
    # most important
    expect_eq_no_msg(
        match_citations(df_all, df_NA, add_col = "match"),
        dplyr::mutate(df_all, match = 1:length(pub_id_types))
    )
})
