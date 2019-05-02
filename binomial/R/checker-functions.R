#checker functions



#check_prob function that verifies that probability inputs prob are valid that is 0 leq prob geq 1.
check_prob <- function(prob) {
  if (class(prob) != "numeric") {
    stop("prob input must be numeric")
  }

  else if (prob <= 1 & prob >= 0) {
    return(TRUE)
  }
  else {
    stop("prob input has to be a number between 0 and 1")
  }

}


#check_trials function checks the value of the input trials and returns TRUE if it is a non-negative integer, else stops with error message.

check_trials <- function(trials) {
  if (class(trials) != "numeric") {
    stop("trials input must be numeric")
  }

  else if (trials == as.integer(trials) & trials >= 0) {
    return(TRUE)
  }
  else {
    stop("Invalid trials input. Trials must be a non-negative integer.")
  }

}

#check_success checks that the success input is valid non-negative integer greater than or equal to 0
# and less than or equal to the number of trials.

check_success <- function(success, trials) {
  if (class(success) != "numeric" & class(success) != "integer") {
    stop("success input must be numeric")
  }

  else if (!all(success == as.integer(success)) | !all(success >= 0)){
    stop("Success must be an non-negative integer vector input.")
  }

  else if (all(success <= trials)) {
    return(TRUE)
  }
  else  {
    stop("Success input cannot be larger than the trials input")
  }
}
