# collapse_to_string() tests --------------------------------------------------

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

test_that("collapse_to_string(): single vectors are concatenated", {
    expect_identical(collapse_to_string(df$dbl), "1|2.5")
    expect_identical(collapse_to_string(df$int), "1|2")
    expect_identical(collapse_to_string(df$lgl), "TRUE|FALSE")
    expect_identical(collapse_to_string(df$chr), "a|b")
    expect_identical(collapse_to_string(df$fac), "a|b")
    expect_identical(collapse_to_string(df$dat), "2021-12-02|2021-12-03")
    expect_identical(
        collapse_to_string(df$tim),
        "2021-12-01 17:31:59|2021-12-01 17:32:00"
    )
})

test_that("collapse_to_string(): delim is used", {
    expect_identical(collapse_to_string(df$int, delim = ""), "12")
    expect_identical(collapse_to_string(df$int, delim = "."), "1.2")
})

test_that("collapse_to_string(): base R structured data types are concatenated", {
    expect_identical(collapse_to_string(d), "1|2.5|a|b")
    expect_identical(collapse_to_string(m), "1|2.5|a|b")
    expect_identical(collapse_to_string(l), "1|2.5|a|b")
    expect_identical(collapse_to_string(nl), "1|2.5|a|b|TRUE|FALSE")
})


# collapse_col*() tests ---------------------------------------------------

#### Input ####
cc_df <- tibble::tibble(
     x = c(1, 2, 3, 3, 4, 4, 4),
     y = c("a", "a", "b", "b", "c", "c", "e"),
     z = c("Z", "Y", "X", "X", "W", "V", "U")
)

# Expected results for unique method (should be the same for both fxns)
x_unique <- tibble::tibble(
    # NOTE:
    #   1. collapsed cols converted to chr (even if nothing to paste together).
    #   2. group_by reorders by uncollapsed cols - combination of y + z.
    x = c("2", "1", "3", "4", "4", "4"),
    y = c("a", "a", "b", "c", "c", "e"),
    z = c("Y", "Z", "X", "V", "W", "U")
)

z_unique <- tibble::tibble(
    x = c(1, 2, 3, 4, 4),
    y = c("a", "a", "b", "c", "e"),
    z = c("Z", "Y", "X", "W|V", "U")
)

xz_unique <- tibble::tibble(
    x = c("1|2", "3", "4", "4"),
    y = c("a", "b", "c", "e"),
    z = c("Z|Y", "X", "W|V", "U")
)

yz_unique <- tibble::tibble(
    x = c(1, 2, 3, 4),
    y = c("a", "a", "b", "c|e"),
    z = c("Z", "Y", "X", "W|V|U")
)


#### Custom expect function ####

# Expects a specified number of unique rows in df
expect_unique_rows <- function(object, n) {
    # 1. Capture object and label
    act <- quasi_label(rlang::enquo(object), arg = "object")

    # 2. Call expect()
    act$n <- act$val %>%
        unique() %>%
        nrow()

    expect(
        act$n == n,
        sprintf("%s has %i unique rows, not %i.", act$lab, act$n, n)
    )

    # 3. Invisibly return the value
    invisible(act$val)
}

# first method
z_first <- tibble::tibble(
    x = c(1, 2, 3, 4, 4),
    y = c("a", "a", "b", "c", "e"),
    z = c("Z", "Y", "X", "W", "U")
)
yz_first <- tibble::tibble(
    x = c(1, 2, 3, 4),
    y = c("a", "a", "b", "c"),
    z = c("Z", "Y", "X", "W")
)


# last method
z_last <- tibble::tibble(
    x = c(1, 2, 3, 4, 4),
    y = c("a", "a", "b", "c", "e"),
    z = c("Z", "Y", "X", "V", "U")
)
yz_last <- tibble::tibble(
    x = c(1, 2, 3, 4),
    y = c("a", "a", "b", "e"),
    z = c("Z", "Y", "X", "U")
)

#### collapse_col() ####

test_that("collapse_col() works", {
    # tests
    expect_identical(collapse_col(cc_df, x), x_unique)
    expect_identical(collapse_col(cc_df, z), z_unique)
    expect_identical(collapse_col(cc_df, c(x, z)), xz_unique)
    expect_identical(collapse_col(cc_df, c(y, z)), yz_unique)
})

test_that("collapse_col(): duplicated rows (3-4) are always collapsed", {
    expect_unique_rows(collapse_col(cc_df, y), nrow(cc_df) - 1)
})

test_that("collapse_col(method = 'first') works", {
    expect_identical(collapse_col(cc_df, z, method = 'first'), z_first)
    expect_identical(collapse_col(cc_df, c(y, z), method = 'first'), yz_first)
})

test_that("collapse_col(method = 'last') works", {
    expect_identical(collapse_col(cc_df, z, method = 'last'), z_last)
    expect_identical(collapse_col(cc_df, c(y, z), method = 'last'), yz_last)
})


#### collapse_col_flex() ####

test_that("collapse_col_flex(): duplicated rows (3-4) are always collapsed", {
    expect_unique_rows(
        collapse_col_flex(cc_df, y, method = "unique"),
        nrow(cc_df) - 1
    )
    expect_unique_rows(
        collapse_col_flex(cc_df, y, method = "first"),
        nrow(cc_df) - 1
    )
    expect_unique_rows(
        collapse_col_flex(cc_df, y, method = "last"),
        nrow(cc_df) - 1
    )
})


#- individual methods work as expected for 1+ columns -#

test_that("collapse_col_flex() methods work for character types", {

    # unique method
    expect_identical(
        collapse_col_flex(cc_df, z, method = "unique"),
        z_unique
    )
    expect_identical(
        collapse_col_flex(cc_df, y, z, method = "unique"),
        yz_unique
    )

    expect_identical(collapse_col_flex(cc_df, z, method = "first"), z_first)
    expect_identical(collapse_col_flex(cc_df, y, z, method = "first"), yz_first)

    expect_identical(collapse_col_flex(cc_df, z, method = "last"), z_last)
    expect_identical(collapse_col_flex(cc_df, y, z, method = "last"), yz_last)
})


test_that("collapse_col_flex() methods work for numeric types", {
    # unique method
    expect_identical(
        collapse_col_flex(cc_df, x, method = "unique"),
        x_unique
    )

    x_fl <- tibble::tibble(
        # no string conversion with first/last methods, still reordered
        x = c(2, 1, 3, 4, 4, 4),
        y = c("a", "a", "b", "c", "c", "e"),
        z = c("Y", "Z", "X", "V", "W", "U")
    )

    # first method
    expect_identical(collapse_col_flex(cc_df, x, method = "first"), x_fl)

    # last method
    expect_identical(collapse_col_flex(cc_df, x, method = "last"), x_fl)
})


test_that("collapse_col_flex() methods work for character & numeric types", {
    # NOTE: collapse x & z to keep only unique y values
    # unique method
    expect_identical(
        collapse_col_flex(cc_df, x, z, method = "unique"),
        xz_unique
    )

    # first method
    xz_first <- tibble::tibble(
        x = c(1, 3, 4, 4),
        y = c("a", "b", "c", "e"),
        z = c("Z", "X", "W", "U")
    )
    expect_identical(collapse_col_flex(cc_df, x, z, method = "first"), xz_first)

    # last method
    xz_last <- tibble::tibble(
        x = c(2, 3, 4, 4),
        y = c("a", "b", "c", "e"),
        z = c("Y", "X", "V", "U")
    )
    expect_identical(collapse_col_flex(cc_df, x, z, method = "last"), xz_last)
})


#- mixed methods work as expected for 1+ columns -#

test_that("collapse_col_flex() mixed methods work", {
    # style of method specification works (unique only)
    expect_identical(
        collapse_col_flex(cc_df, x = "unique", z = "unique"),
        xz_unique
    )

    # first + unique methods
    xf_zu <- tibble::tibble(
        x = c(1, 3, 4, 4),
        y = c("a", "b", "c", "e"),
        z = c("Z|Y", "X", "W|V", "U")
    )
    expect_identical(collapse_col_flex(cc_df, x = "first", z = "unique"), xf_zu)

    # first + last methods
    xf_zl <- tibble::tibble(
        x = c(1, 3, 4, 4),
        y = c("a", "b", "c", "e"),
        z = c("Y", "X", "V", "U")
    )
    expect_identical(collapse_col_flex(cc_df, x = "first", z = "last"), xf_zl)
})
