test_that("colnum_to_ss_letter() works", {
  expect_equal(colnum_to_ss_letter(1), "A")
  expect_equal(colnum_to_ss_letter(26), "Z")
  expect_equal(colnum_to_ss_letter(27), "AA")
  expect_equal(colnum_to_ss_letter(52), "AZ")
  expect_equal(colnum_to_ss_letter(703), "AAA")
})
