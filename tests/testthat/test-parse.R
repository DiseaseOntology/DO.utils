test_that("parse_html_table() works", {
    expect_snapshot(parse_html_table("data/html/table1.html"))
})

test_that("parse_html_table() `type` arg works", {
    expect_snapshot(
        parse_html_table("data/html/mattable1.html", type = "mat-table")
    )
    expect_snapshot(
        parse_html_table("data/html/mattable1.html", type = "unknown")
    )
    expect_snapshot(
        parse_html_table("data/html/table1.html", type = "unknown")
    )
})

test_that("parse_html_table() `n` arg works", {
    expect_snapshot(
        parse_html_table("data/html/table3.html", type = "unknown", n = 3L)
    )
    expect_snapshot(
        parse_html_table("data/html/mattable2.html", type = "unknown", n = 2L)
    )
    expect_error(
        parse_html_table("data/html/mattable2.html", type = "unknown", n = 4L)
    )
})
