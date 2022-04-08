# extract_pmid() ----------------------------------------------------------

##### DATA #####
elink_list_full <- readRDS("data/elink_list_full.rds") # 2 elements both w/results
elink_list <- readRDS("data/elink_list.rds") # 3 elements, only 1st w/results
elink <- elink_list[[1]]
elink0 <- elink_list[[2]]

##### TESTS #####
test_that("extract_pmid elink method works", {
    expect_error(extract_pmid(elink0), class = "no_result")
    expect_equal(extract_pmid(elink), c("35380658", "33594374"))
})


test_that("extract_pmid elink_list method works", {
    expect_snapshot(extract_pmid(elink_list)) # default = warning
    expect_snapshot(extract_pmid(elink_list, no_result = "message"))
    expect_snapshot(
        error = TRUE,
        extract_pmid(elink_list, no_result = "error")
    )
    expect_equal(
        extract_pmid(elink_list, no_result = "none"),
        list("34755882" = c("35380658", "33594374"))
    )
})


test_that("extract_pmid.elink_list does NOT produce condition when all elements have results", {
    expect_condition(regexp = NA, extract_pmid(elink_list_full))
    expect_condition(
        regexp = NA,
        extract_pmid(elink_list_full, no_result = "message")
    )
    expect_condition(
        regexp = NA,
        extract_pmid(elink_list_full, no_result = "error")
    )
})


# extract_robots_delay() tests --------------------------------------------

# data
rt_txt <- readr::read_file("data/fake_robots.txt")
robxp <- spiderbar::robxp(rt_txt)

# tests
test_that("extract_robots_delay() uses '*'", {
    expect_equal(extract_robots_delay(robxp, user_agent = "*"), 10)

    # should use it if specified user agent is not in robots.txt
    expect_equal(extract_robots_delay(robxp, user_agent = "fake"), 10)
})

test_that("extract_robots_delay() works for DO.utils", {
    expect_equal(extract_robots_delay(robxp), 5)
})
