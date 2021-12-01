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
        class = c("POSIXct", "POSIXt")
    ),
    stringsAsFactors = FALSE
)

test_that("single vectors are concatenated", {
    expect_identical(cast_to_string(df$dbl), "1|2.5")
    expect_identical(cast_to_string(df$int), "1|2")
    expect_identical(cast_to_string(df$lgl), "TRUE|FALSE")
    expect_identical(cast_to_string(df$chr), "a|b")
    expect_identical(cast_to_string(df$fac), "a|b")
    expect_identical(cast_to_string(df$dat), "2021-12-02|2021-12-03")
    expect_identical(
        cast_to_string(df$tim),
        "2021-12-01 12:31:59|2021-12-01 12:32:00"
    )
})

test_that("delim is used", {
    expect_identical(cast_to_string(df$int, delim = ""), "12")
    expect_identical(cast_to_string(df$int, delim = "."), "1.2")
})
