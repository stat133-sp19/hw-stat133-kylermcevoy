

context("aux summary measures")

test_that("aux_mean output makes sense", {

  expect_equal(aux_mean(3, 0.5), 1.5)
  expect_equal(aux_mean(10, 0.7), 7)
  expect_length(aux_mean(15, 0.6), 1)
  expect_error(aux_mean("a", 6))

})

test_that("aux_variance output makes sense", {

  expect_equal(aux_variance(3, 0.5), 0.75)
  expect_equal(aux_variance(10, 0.25), 1.875)
  expect_length(aux_variance(10, 0.5), 1)
  expect_error(aux_variance("d", 0.5))


})

test_that("aux_mode output makes sense", {

  expect_length(aux_mode(3, 0.5), 2)
  expect_length(aux_mode(6, 0.5), 1)
  expect_equal(aux_mode(5, 0.5), c(3, 2))
  expect_equal(aux_mode(6, 0.5), 3)
  expect_error(aux_mode("a", 0.5))


})

test_that("aux_skewness output makes sense", {
  expect_length(aux_skewness(3, 0.5), 1)
  expect_error(aux_skewness("a", 0.5))
  expect_equal(aux_skewness(7, 0.5), 0)
  expect_equal(aux_skewness(10, 1), -Inf)
  expect_type(aux_skewness(10, 0.6), 'double')


})

test_that("aux_kurtosis output makes sense", {
  expect_length(aux_kurtosis(3, 0.5), 1)
  expect_error(aux_kurtosis("a", 0.5))
  expect_type(aux_kurtosis(10, 0.6), 'double')


})
