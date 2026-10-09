test_that("mu is returned correctly", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$mu, 10)
})

test_that("sigma is returned correctly", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$sigma, 5)
})

test_that("a is returned correctly", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$a, 6)
})

test_that("z is the z-score of a", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$z, -0.8)
})

test_that("prob is P(X <= a)", {
  result = myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(result$prob, round(pnorm(6, mean = 10, sd = 5), 4))
})

test_that("bad sigma gives an error", {
  expect_error(myncurve(mu = 10, sigma = -5, a = 6))
})
