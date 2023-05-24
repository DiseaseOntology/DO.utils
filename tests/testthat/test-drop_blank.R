test_that("drop_blank.character() works", {
    expect_identical(drop_blank(c("", "A")), "A")
    expect_identical(drop_blank(c("A", "B")), c("A", "B"))
})

test_that("drop_blank.list() works", {
    w_blank <-
    expect_identical(
        drop_blank(
            list(
                c("", "A"),
                c("A", "B"),
                c("C", "D", "", "E", "")
            )
        ),
        list(
            c("A"),
            c("A", "B"),
            c("C", "D", "E")
        )
    )
})
