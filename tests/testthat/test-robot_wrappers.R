test_owl <- "data/repo/src/ontology/doid-edit.owl"

test_that("robot_query() works with ASK queries", {
    skip_no_robot()
    qt <- system.file("sparql", "tests", "ask-true.rq", package = "DO.utils")
    qf <- system.file("sparql", "tests", "ask-false.rq", package = "DO.utils")
    out_temp <- tempfile(fileext = ".txt")

    expect_true(robot_query(test_owl, qt))
    expect_false(robot_query(test_owl, qf))
    expect_equal(robot_query(test_owl, qt, out_temp), out_temp)
})

test_that("robot_query() works with SELECT queries", {
    skip_no_robot()
    qs <- system.file("sparql", "tests", "select.rq", package = "DO.utils")
    out_temp <- tempfile(fileext = ".tsv")
    out_df <- structure(
        list(`?id` = "DOID:4", `?label` = "disease"),
        row.names = c(NA, -1L),
        class = c("tbl_df", "tbl", "data.frame")
    )

    expect_equal(robot_query(test_owl, qs, tidy_what = "nothing"), out_df)
    expect_equal(robot_query(test_owl, qs, out_temp), out_temp)
})

test_that("robot_query() works with CONSTRUCT queries", {
    skip_no_robot()
    qc <- system.file("sparql", "tests", "construct.rq", package = "DO.utils")
    out_temp <- tempfile(fileext = ".owl")

    expect_error(robot_query(test_owl, qc), "output.*CONSTRUCT")
    expect_equal(robot_query(test_owl, qc, out_temp), out_temp)
})

# test_that("robot_query() works with UPDATE queries", {
#     # no tests yet
# })
