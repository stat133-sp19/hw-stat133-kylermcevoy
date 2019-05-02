#Main Functions


#' @title The Choose Function
#' @description bin_choose() takes one value for trials and multiple values for sucesses and returns the choose function for trials choose successes
#' @param n (integer) the number of trials
#' @param k (integer) a vector of the number of successes - -can be a single value or multiple values. Must be less than n.
#' @return function returns the value of n choose k for each value k provided
#' @export
#' @examples
#' bin_choose(n = 5, k = 2)
#' bin_choose(10, 4)
#' bin_choose(n = 20, k = c(5, 10, 15))

bin_choose <- function(n, k) {
  if (!all(k <= n)) {
    stop("k must be less than or equal to n")
  }
  else {
    return(factorial(n) / ((factorial(n - k) * factorial(k))))
  }
}

#' @title Binomial Probabilities
#' @description bin_probability() takes one or more number of successes, one value of trials and one value for probability and returns the probability of that number of successes in that many trials for a binomial distribution
#' @param success (integer) the number of successes whose probability you'd like to calculate
#' @param trials (integer) the total number of trials for yoru binomial distribution
#' @param prob (numeric) probability value between 0 and 1 that gives the probability of success per trial
#' @return (numeric) returns the probability from a binomial distribution PMF value for each number of successes inputted
#' @export
#' @examples
#' bin_probability(success = 8, trials = 10, prob = 0.6)
#' bin_probability(success = c(3, 6 ,9), trials = 12, prob = 0.25)
#' bin_probability(6, 15, 0.5)

bin_probability <- function(success, trials, prob) {
  if (check_trials(trials) != TRUE) {
    stop("invalid number of trials")
  }
  else if (check_prob(prob) != TRUE) {
    stop("prob must be a number between 0 and 1")
  }
  else if (check_success(success, trials) != TRUE) {
    stop("success must be a vector of integer values less than trials")
  }
  else {
    return(bin_choose(n = trials, k = success) * (prob ^ success) * ( (1 - prob)^ (trials - success)))
  }

}


#' @title Binomial Distribution
#' @description given a number of trials and a probability of success produces a data.frame with class bindis that contains the probability of each number of successes
#' @param trials (integer) the number of trials for the binomial distribution you want to calculate the distribution of
#' @param prob (double) a number giving the probability of success on each trial for the binomial distribution
#' @return a data.frame with class "bindis" and "data.frame" that contains a column for the number of successes from 0 to trials and a column for the probability of observing that number of successes in the given number of trials
#' @export
#' @examples
#' bin_distribution(15, 0.5)
#' bin_distribution(trials = 5, prob = 0.75)

bin_distribution <- function(trials, prob) {
  success_vec <- 0:trials
  prob_vec <- bin_probability(success = success_vec, trials = trials, prob = prob)
  bindis <- data.frame(success = success_vec, probability = prob_vec)
  class(bindis) <- c("bindis", "data.frame")
  return(bindis)
}


#' @export

plot.bindis <- function(x, ...) {
  graphics::barplot(height = x$probability, xlab = "Number of Successes", ylab = "Probability", names.arg = x$success, col = "#00FFB3")
}


#' @title Binomial Cumulative Distribution
#' @description given a number of trials and a probability of success produces a data.frame with class bindis that contains the probability of each number of successes
#' @param trials (integer) the number of trials for the binomial distribution you want to calculate the distribution of
#' @param prob (double) a number giving the probability of success on each trial for the binomial distribution
#' @return a data.frame with class "bincum" and "data.frame" that contains a column for the number of successes from 0 to trials, a column for the probability of observing that number of successes in the given number of trials, and a column for the cumulative probability of seeing that number of successes or fewer in the given trials.
#' @export
#' @examples
#' bin_cumulative(15, 0.5)
#' bin_cumulative(trials = 5, prob = 0.75)

bin_cumulative <- function(trials, prob) {
  success_vec <- 0:trials
  prob_vec <- bin_probability(success = success_vec, trials = trials, prob = prob)
  cum_sum_vec <- cumsum(prob_vec)
  bincum <- data.frame(success = success_vec, probability = prob_vec, cumulative = cum_sum_vec)
  class(bincum) <- c("bincum", "data.frame")
  return(bincum)
}

#' @export

plot.bincum  <- function(x, ...) {
  graphics::plot(x = x$success, y = x$cumulative, type = "b", xlab = "Number of Successes", ylab = "Probability of equal or fewer Successes")

}

#' @title Binomial Random Variable
#' @description bin_variable() function creates an object of class binvar that stores information on
#' the number of trials and prob of success of a binomial RV
#' @param trials (numeric integer) takes a non-negative integer for the number of trials for the binomial RV
#' @param prob (numeric) values from 0 to 1. Gives probability of success per trial for a binomial RV
#' @return object of class binvar that records the number of trials and probability of success.
#' @export
#' @examples
#' bin_variable(trials = 5, prob = 0.5)
#' bin_variable(20, 0.7)

bin_variable <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  binvar <- list(trials = trials, prob = prob)
  class(binvar) <- "binvar"
  return(binvar)
}

#' @export

print.binvar <- function(x, ...) {
  print("Binomial variable")
  cat("\n")
  print(noquote("Parameters"))
  print(noquote(paste("- number of trials:", x$trials)))
  print(noquote(paste("- prob of success:", x$prob)))
  invisible(x)

}

#' @export

summary.binvar <- function(object, ...) {
  binsum = list(
    trials = object$trials,
    prob = object$prob,
    mean = aux_mean(object$trials, object$prob),
    variance = aux_variance(object$trials, object$prob),
    mode = aux_mode(object$trials, object$prob),
    skewness = aux_skewness(object$trials, object$prob),
    kurtosis = aux_kurtosis(object$trials, object$prob))

  class(binsum) = c("summary.binvar")
  return(binsum)


}

#' @export

print.summary.binvar <- function(x, ...) {
  print("Summary Binomial")
  cat("\n")
  print(noquote("Parameters"))
  print(noquote(paste("- number of trials:", x$trials)))
  print(noquote(paste("- prob of success :", x$prob)))
  cat("\n")
  print(noquote("Measures"))
  print(noquote(paste("- mean      :", x$mean)))
  print(noquote(paste("- variance  :", x$variance)))
  print(noquote(paste("- mode      :", x$mode)))
  print(noquote(paste("- skewness  :", x$skewness)))
  print(noquote(paste("- kurtosis  :", x$kurtosis)))

}



#' @title Binomial Mean
#' @description returns the mean of the binomial distribution for the given number of trials and probability of success per trial
#' @param trials (integer) the number of trials for your binomial distribution
#' @param prob (numeric) numeric value between 0 and 1, gives probability of success per trial
#' @return numeric value for the mean of the binomial distribution with input parameters
#' @export
#' @examples
#' bin_mean(20, 0.5)
#' bin_mean(trials = 10, prob = 0.25)

bin_mean <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  if (check_trials(trials) & check_prob(prob)) {
    return(aux_mean(trials, prob))
  }
}

#' @title Binomial Variance
#' @description returns the variance of the binomial distribution for the given number of trials and probability of success per trial
#' @param trials (integer) the number of trials for your binomial distribution
#' @param prob (numeric) numeric value between 0 and 1, gives probability of success per trial
#' @return numeric value for the variance of the binomial distribution with input parameters
#' @export
#' @examples
#' bin_variance(20, 0.5)
#' bin_variance(trials = 10, prob = 0.25)

bin_variance <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  if (check_trials(trials) & check_prob(prob)){
    return(aux_variance(trials, prob))
  }
}

#' @title Binomial Mode
#' @description returns the mode of the binomial distribution for the given number of trials and probability of success per trial
#' @param trials (integer) the number of trials for your binomial distribution
#' @param prob (numeric) numeric value between 0 and 1, gives probability of success per trial
#' @return numeric value for the mode of the binomial distribution with input parameters. Will return two values for mode if prob * trials + trials is an integer
#' @export
#' @examples
#' bin_mode(20, 0.5)
#' bin_mode(trials = 10, prob = 0.25)

bin_mode <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  if (check_trials(trials) & check_prob(prob)){
    return(aux_mode(trials, prob))
  }
}



#' @title Binomial skewness
#' @description returns the skewness of the binomial distribution for the given number of trials and probability of success per trial
#' @param trials (integer) the number of trials for your binomial distribution
#' @param prob (numeric) numeric value between 0 and 1, gives probability of success per trial
#' @return numeric value for the skewness of the binomial distribution with input parameters
#' @export
#' @examples
#' bin_skewness(20, 0.5)
#' bin_skewness(trials = 10, prob = 0.25)

bin_skewness <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  if (check_trials(trials) & check_prob(prob)){
    return(aux_skewness(trials, prob))
  }
}


#' @title Binomial Kurtosis
#' @description returns the Kurtosis of the binomial distribution for the given number of trials and probability of success per trial
#' @param trials (integer) the number of trials for your binomial distribution
#' @param prob (numeric) numeric value between 0 and 1, gives probability of success per trial
#' @return numeric value for the kurtosis of the binomial distribution with input parameters
#' @export
#' @examples
#' bin_kurtosis(20, 0.5)
#' bin_kurtosis(trials = 10, prob = 0.25)

bin_kurtosis <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)
  if (check_trials(trials) & check_prob(prob)){
    return(aux_kurtosis(trials, prob))
  }
}



