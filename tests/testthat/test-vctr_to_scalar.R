
# vctr_to_string() --------------------------------------------------------

test_that("vctr_to_string() works", {
    expect_identical(vctr_to_string(c("a", "b")), "a|b")
    expect_identical(vctr_to_string(c(NA, "a")), "NA|a")
    expect_identical(vctr_to_string(NA), NA_character_)
})

test_that("vctr_to_string() na.rm works", {
    expect_identical(vctr_to_string(NA, na.rm = TRUE), NA_character_)
    expect_identical(vctr_to_string(c(NA, "a"), na.rm = TRUE), "a")
})
