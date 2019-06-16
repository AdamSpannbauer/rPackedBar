context("guess bar count")

test_that("guess bar count", {
  x = c(110, 100, 85, 85, rep(1, 100))
  guess = guess_bar_count(x, min_bar = 3, max_bar = 25)
  expect_equal(guess, 4L)
})
