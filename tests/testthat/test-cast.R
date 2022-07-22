# cast_to_string() tests --------------------------------------------------

# data.frame of all major types
df <- data.frame(
    dbl = c(1, 2.5),
    int = 1:2,
    lgl = c(T, F),
    chr = c("a", "b"),
    fac = factor(c("a", "b")),
    dat = structure(c(18963, 18964), class = "Date"),
    tim = structure(
        c(1638379919.66874, 1638379920.66874),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
    ),
    stringsAsFactors = FALSE
)

# other complex data types (no plan for array)
d <- df[, c("dbl", "chr")]
m <- matrix(c(as.character(df$dbl), df$chr), nrow = 2)
l <- as.list(d)
nl <- list(l, df$lgl)

test_that("single vectors are concatenated", {
    expect_identical(cast_to_string(df$dbl), "1|2.5")
    expect_identical(cast_to_string(df$int), "1|2")
    expect_identical(cast_to_string(df$lgl), "TRUE|FALSE")
    expect_identical(cast_to_string(df$chr), "a|b")
    expect_identical(cast_to_string(df$fac), "a|b")
    expect_identical(cast_to_string(df$dat), "2021-12-02|2021-12-03")
    expect_identical(
        cast_to_string(df$tim),
        "2021-12-01 17:31:59|2021-12-01 17:32:00"
    )
})

test_that("delim is used", {
    expect_identical(cast_to_string(df$int, delim = ""), "12")
    expect_identical(cast_to_string(df$int, delim = "."), "1.2")
})

test_that("complex data types are concatenated", {
    expect_identical(cast_to_string(d), "1|2.5|a|b")
    expect_identical(cast_to_string(m), "1|2.5|a|b")
    expect_identical(cast_to_string(l), "1|2.5|a|b")
    expect_identical(cast_to_string(nl), "1|2.5|a|b|TRUE|FALSE")
})


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

test_that("cast_to_range() works when a 2-long range starts at end", {
    almost_range_at_end <- c(x, 46)
    txt_version <- paste0(almost_range_at_end, "txt")

    expect_error(cast_to_range(almost_range_at_end), regexp = NA)
    expect_error(
        cast_to_range(
            txt_version,
            ~ as.integer(stringr::str_remove(.x, "txt")),
            start_rm = "txt"
        ),
        regexp = NA
    )
})
