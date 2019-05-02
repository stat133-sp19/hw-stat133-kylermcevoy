

context("Main Function Tests of binomial")

test_that("bin_choose rejects invalid input and outputs correctly",
          {
            expect_error(bin_choose(n = 4, k = 6))
            expect_error(bin_choose(n = 5, k = 4:6))
            expect_equal(bin_choose(n = 6, k =5), 6)
            expect_equal(bin_choose(n = 5, k = 5), 1)
            expect_equal(bin_choose(n = 5, k = 0), 1)
            expect_equal(bin_choose(n = 20, k = 2), 190)
            expect_length(bin_choose(n = 20, k = 1:4), 4)

          })

test_that("bin_probability() takes correct input, errors bad input", {

        expect_error(bin_probability(trials = 5, success = 6, prob = 0.5))
        expect_error(bin_probability(trials = 5, success = 1, prob = 1.5))
        expect_equal(bin_probability(trials = 5, success = 1, prob = 0.5), 0.15625)
        expect_length(bin_probability(trials = 5, success = 0:5, prob = 0.5), 6)
        expect_error(bin_probability(trials = 5, success = 4:6, prob = 0.5))


})


test_that("bin_distribution produces bindis dataframe from good input errors with bad input", {

        expect_s3_class(bin_distribution(trials = 20, prob = 0.5), class = c("bindis", "data.frame"))
        expect_error(bin_distribution(trials = 20, prob = 1.5))
        expect_error(bin_distribution(trials = 20.5, prob = 0.5))
        expect_equal(bin_distribution(trials = 20, prob = 0.7)$probability, bin_probability(trials = 20, prob = 0.7, success = 0:20))
})


test_that("bin_cumulative has correct class, calls error from checker functions", {

  expect_s3_class(bin_cumulative(trials = 20, prob = 0.5), class = c("bincum", "data.frame"))
  expect_error(bin_cumulative(trials = 20.5, prob = 0.5))
  expect_error(bin_cumulative(trials = 20, prob = 1.2))
  expect_length(bin_cumulative(trials = 25, prob = 0.6)$success, 26)
  expect_equal(bin_cumulative(trials = 20, prob = 0.7)$cumulative, cumsum(bin_probability(trials = 20, success = 0:20, prob = 0.7)))


})

test_that("bin_variable creates class binvar object and errors properly from input checks", {

  expect_s3_class(bin_variable(trials = 5, prob = 0.5), class = "binvar")
  expect_error(bin_variable(trials = 5.5, prob = 0.5))
  expect_error(bin_variable(trials = 5, prob = 1.5))
  expect_equal(bin_variable(trials = 5, prob = 0.5)$prob, 0.5)
  expect_equal(bin_variable(trials = 20, prob = 0.6)$trials, 20)


})
