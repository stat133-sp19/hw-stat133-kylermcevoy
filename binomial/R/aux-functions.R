#auxiliary private functions



#aux_mean function returns the mean of the binomial distribution for a given number of trials and the probability of success per trial, prob.

aux_mean <- function(trials, prob) {
  return(trials * prob)
}

#aux_variance function returns the variance of the binomial distribution for the given parameters trials and probability of success per trial, prob.

aux_variance <- function(trials, prob) {
  return(trials * prob * (1 - prob))
}

#aux_mode function returns the mode of the binomial distribution with the given number of trials and probability (prob) of success.
#will return integer floor of trials * prob + prob. The mode will be both m and m - 1 in the case that m = trials * prob + prob is an integer.

aux_mode <- function(trials, prob) {
  if(floor(trials * prob + prob) == trials * prob + prob) {
    return(c(trials * prob + prob, trials * prob + prob - 1))
  }
  else {
    return(floor(trials * prob + prob))
  }
}

#aux_skewness functions returns the skewness of the binomial distribution with the specified number of trials and probability of success, prob

aux_skewness <- function(trials, prob) {
  return( (1 - 2 * prob) / (sqrt(trials * prob * (1 - prob))))
}

#aux_kurtosis function returns the kurtosis of the binomial distribution
#with parameters trials and prob

aux_kurtosis <- function(trials, prob) {
  return( (1 - 6 * prob * (1 - prob)) / (trials * prob * (1 - prob)))

}
