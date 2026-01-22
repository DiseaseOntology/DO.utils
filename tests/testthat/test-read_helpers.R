# guess_delim() tests -----------------------------------------------------

test_that("guess_delim() works", {
    expect_error(guess_delim(""))

    expect_equal(guess_delim(c("a,b", "c,d", "e,f")), ",")
    expect_equal(guess_delim(c("a\tb", "c\td", "e\tf")), "\t")

    expect_error(guess_delim(c("a,b", "c,d", "e\tf")))
})

test_that("guess_delim(strict = FALSE) works", {
    expect_error(guess_delim("", strict = FALSE))
    expect_error(guess_delim(c("", "a,b"), strict = FALSE))
    expect_error(guess_delim(c("a,b", "c\td"), strict = FALSE))

    expect_equal(guess_delim(c("a,b", "c,d", ""), strict = FALSE), ",")
    expect_equal(guess_delim(c("a,b", "c,d", "ef"), strict = FALSE), ",")
    expect_equal(guess_delim(c("a,b", "c,d", "e\tf"), strict = FALSE), ",")

    expect_equal(guess_delim(c("a\tb", "c\td", "e,f"), strict = FALSE), "\t")
    expect_equal(guess_delim(c("a\tb", "c\td", "ef"), strict = FALSE), "\t")
})

test_that("guess_delim() handles quoted data", {
    expect_equal(guess_delim(c("a,b", "c,d", 'e,"f\tg"')), ",")
    expect_equal(guess_delim(c("a\tb", "c\td", 'e\t"f,g"')), "\t")

    expect_error(guess_delim(c("a,b", "c,d", '"e\tf"')))
    expect_equal(guess_delim(c("a,b", "c,d", '"e\tf"'), strict = FALSE), ",")
})


test_that("guess_delim() works with string data", {
    expect_equal(guess_delim("a,b\nc,d\ne,f"), ",")
    expect_equal(guess_delim("a,b\r\nc,d\r\ne,f"), ",")
})

test_that("guess_delim() works with multi-line character data", {
    expect_equal(guess_delim(c("a,b\nc,d", "e,f\ng,h")), ",")
    expect_equal(guess_delim(c("a\tb\r\nc\td", "e\tf\r\ng\th")), "\t")

    expect_error(guess_delim(c("a,b\nc,d", "e\tf\r\ng\th")))
    expect_error(guess_delim(c("a,b\nc,d", "e\tf\r\ng\th", "i,j")))
    expect_equal(
        guess_delim(c("a,b\nc,d", "e\tf\r\ng\th", "i,j"), strict = FALSE),
        ","
    )

    expect_error(guess_delim(c("a,b\nc,d", '"e,f"\r\ng\th')))
    expect_error(guess_delim(c("a,b\nc,d", '"e,f"\r\ng\th', "i,j")))
    expect_equal(
        guess_delim(c("a,b\nc,d", '"e,f"\r\ng\th', "i,j"), strict = FALSE),
        ","
    )
})
