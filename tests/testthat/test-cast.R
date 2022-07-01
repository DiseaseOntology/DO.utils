# cast_to_range() tests ---------------------------------------------------

x <- c(1:2, 8:6, 4, -1:-2, 20:37, 4, 40, 43, 45)
txt <- paste0(x, "txt")

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

test_that("cast_to_range() NA values are removed", {
    expect_identical(
        cast_to_range(c(NA, x, NA)),
        "-2,-1,1,2,4,6-8,20-37,40,43,45"
    )
})

test_that("cast_to_range()'s start_rm argument works", {
    expect_identical(
        cast_to_range(
            txt,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            start_rm = "txt"
        ),
        "-2txt,-1txt,1txt,2txt,4txt,6-8txt,20-37txt,40txt,43txt,45txt"
    )
})

test_that("cast_to_range()'s end_rm argument works", {
    stxt <- paste0("txt", x)
    expect_identical(
        cast_to_range(
            stxt,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            end_rm = "txt"
        ),
        "txt-2,txt-1,txt1,txt2,txt4,txt6-8,txt20-37,txt40,txt43,txt45"
    )
})
