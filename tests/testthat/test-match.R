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


##### CUSTOM TESTING FUNCTION #####
# suppresses messages when not being tested
expect_eq_no_msg <- function(...) {
    suppressMessages(expect_equal(...))
}

expect_3msg <- function(...) {
    expect_message(expect_message(expect_message(...)))
}


##### TESTS #####
# Snapshots (messages & values) or Results only
test_that("match_citations(): works for all possible w/matches (df only)", {
    expect_snapshot(match_citations(df_all, df_NA))
    expect_snapshot(match_citations(df_all, rev(df_NA)))
})

test_that("match_citations(): works for single, similar type w/matches", {
    # both df: 1 col each
    expect_snapshot(match_citations(df_all[2], df_NA[2]))
    # both df: 2 cols in x but only 1 in ref
    expect_snapshot(match_citations(df_all[1:2], df_NA[2]))
    # both vctr
    expect_snapshot(match_citations(df_all[[3]], df_NA[[3]]))
    # df vs vector, exact match
    expect_eq_no_msg(
        match_citations(df_all[1], df_all[[1]]),
        1:nrow(df_all)
    )
    expect_eq_no_msg(
        match_citations(df_all[[1]], df_all[1]),
        1:nrow(df_all)
    )
})

test_that("match_citations(): works for single, similar type w/NO matches", {
    # --> says matching but no result (make warning?)
    df0 <- df_NA[1]
    df0[1] <- NA
    expect_eq_no_msg(
        match_citations(df_all[1], df0),
        rep(NA_integer_, nrow(df_all))
    )
})

test_that("match_citations(): add_col works", {
    # most important
    expect_eq_no_msg(
        match_citations(df_all, df_NA, add_col = "match"),
        dplyr::mutate(df_all, match = 1:length(pub_id_types))
    )
})


# Errors
test_that("match_citations(): errors for data.frames without same types", {
    expect_error(match_citations(df_all[1], df_all[2]))
})

test_that("match_citations(): errors for vctrs without same types", {
    expect_error(match_citations(df_all[[1]], df_all[[2]]))
})

test_that("match_citations(): errors when df column name != computed type", {
    expect_error(
        match_citations(
            df_all[[1]],
            # wrong name
            purrr::set_names(df_all[[2]], names(df_all)[1])
        )
    )
})

test_that("match_citations(): errors when types are mixed", {
    mix <- dplyr::coalesce(!!!df_NA)
    # both vctrs
    expect_error(match_citations(df_all[[1]], mix))
    expect_error(match_citations(mix, df_all[[1]]))
    # in df
    df_mix <- df_all
    df_mix[2] <- mix
    expect_3msg(expect_error(match_citations(df_all, df_mix)))
})
