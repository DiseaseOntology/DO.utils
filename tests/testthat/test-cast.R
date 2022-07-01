# cast_to_range() tests ---------------------------------------------------

x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)

test_that("cast_to_range() works", {
    expect_identical(cast_to_range(x), "-2,-1,1,2,4,6-8,20-37,40,43,45")
})

test_that("cast_to_range()'s sep argument works", {
    expect_identical(
        cast_to_range(x, sep = c(" ", "%")),
        "-2 -1 1 2 4 6%8 20%37 40 43 45"
    )
})

test_that("cast_to_range()'s int_fn argument works", {
    txt <- paste0(x, "txt")
    to_int <- function(x, y) as.integer(stringr::str_remove(x, y))
    # fn with arguments
    expect_identical(
        cast_to_range(txt, to_int, y = "txt"),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
    # anonymous function
    expect_identical(
        cast_to_range(
            txt,
            function(x) as.integer(stringr::str_remove(x, "txt"))
        ),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
    # rlang-style lambda
    expect_identical(
        cast_to_range(txt, ~ as.integer(stringr::str_remove(.x, "txt"))),
        "-2txt,-1txt,1txt,2txt,4txt,6txt-8txt,20txt-37txt,40txt,43txt,45txt"
    )
})
