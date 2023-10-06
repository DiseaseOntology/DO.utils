test_that("round_up() works", {
    expect_equal(round_up(1.356, 2), 1.36)
    expect_equal(round_up(-1.356, 2), -1.35)
    expect_equal(round_up(1.356, 0), 2)
    expect_equal(round_up(-1.356, 0), -1)
    expect_equal(round_up(1.356, -2), 100)
    expect_equal(round_up(-1.356, -2), 0)
})

test_that("round_down() works", {
    expect_equal(round_down(1.356, 2), 1.35)
    expect_equal(round_down(-1.356, 2), -1.36)
    expect_equal(round_down(1.356, 0), 1)
    expect_equal(round_down(-1.356, 0), -2)
    expect_equal(round_down(1.356, -2), 0)
    expect_equal(round_down(-1.356, -2), -100)
})

test_that("round_zero() works", {
    expect_equal(round_zero(1.356, 2), 1.35)
    expect_equal(round_zero(-1.356, 2), -1.35)
    expect_equal(round_zero(1.356, 0), 1)
    expect_equal(round_zero(-1.356, 0), -1)
    expect_equal(round_zero(1.356, -2), 0)
    expect_equal(round_zero(-1.356, -2), 0)
    expect_equal(round_zero(1356, -2), 1300)
    expect_equal(round_zero(-1356, -2), -1300)
})
