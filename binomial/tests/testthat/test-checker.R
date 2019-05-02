#checker functions

context("Tests for Checker Functions")

test_that("check_prob handles normal inputs", {

  expect_equal(check_prob(0.67), TRUE)
  expect_length(check_prob(0.1), 1)
  expect_error(check_prob(1.5))

})

test_that("check_trials rejects invalid inputs and accepts valid inputs", {

  expect_equal(check_trials(7), TRUE)
  expect_equal(check_trials(0), TRUE)
  expect_error(check_trials("a"))
  expect_error(check_trials(1.5))
  expect_error(check_trials(-3.7))
  expect_length(check_trials(20), 1)


})

test_that("check_success handles good and bad inputs", {

  expect_equal(check_success(7, 10), TRUE)
  expect_equal(check_success(0:20, 20), TRUE)
  expect_error(check_success("a", 10))
  expect_error(check_success(-7, 10))
  expect_error(check_success(6.5, 10))
  expect_error(check_success(11, 10))
  expect_length(check_success(0:20, 20), 1)

})
