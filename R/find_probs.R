
#' Title find_probs()
#'
#' Description: This is a simple wrapper function for finding the probabilities for various distributions.
#'
#'
#' @param bound Vector of length one or two: c(left,right). This is to define the boundaries of the shaded region of the graph.
#'        Notes: "inner" and "two" require both the left and right input.
#' @param type String being any of "normal", "t-dist", "chi-squared", "binomial"
#' @param tail String being any of "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal"
#'        Notes: "left_not_equal" and "left_not_equal" are intended for the "binomial" only.
#'        Notes: "out" and "two" will produce the same output.
#' @param mean Numeric: vector of one value. The mean of the distribution
#' @param sd Numeric: vector of one value. The standard deviation of a distribution.
#' @param df Numeric: vector of one value. The degree of freedom of a distribution.
#' @param prob Numeric: vector of one value. The probability of success in the binomial distribution.
#' @param trials Numeric: vector of one value. The number of trials in the binomial distribution.
#' @param inverse Logical: If true the function will assume bound is a Area, and the function will attempt to find the
#'        quantile that produces a probability of "bound"
#'        Note: This function assumes only "left" as a tail. Imputing other tails will have no effect on the output.
#'
#' @return Numeric: Probability = being the calculated probability of the region supplied. (The cdf)
#'         Numeric: Value = being the calculated value that produces the area supplied in bound. (the quantile)
#' @export
#'
#' @examples
find_probs <- function(bound, type="normal", tail="left", mean = 0, sd = 1, df = 1, prob = .5, trials = 10, inverse = FALSE){
  ## reading in the bounds for the calculations
  if(tail == "left"){
    logic <- TRUE
  }else{
    logic <- FALSE
  }

  ## Calculations on the Normal
  if(type == "normal"){
    #one tailed
    if(tail == "left" | tail == "right"){
      probability <- pnorm(bound,mean,sd,lower.tail = logic)
    }
    #interior
    if(tail == "inner"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pnorm(max(bound),mean,sd,lower.tail = TRUE) - pnorm(min(bound),mean,sd,lower.tail = TRUE)
    }
    #two tailed
    if(tail == "two" | tail == "outer"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pnorm(max(bound),mean,sd,lower.tail = FALSE) + pnorm(min(bound),mean,sd,lower.tail = TRUE)
    }
    # Inverse
    if(inverse == TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qnorm(bound,mean,sd,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## Calculations on the t-distribution
  if(type == "t-dist"){
    #one tailed
    if(tail == "left" | tail == "right"){
      probability <- pt(bound,df,lower.tail = logic)
    }
    #interior
    if(tail == "inner"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pt(max(bound),df,lower.tail = TRUE) - pt(min(bound),df,lower.tail = TRUE)
    }
    #two tailed
    if(tail == "two" | tail == "outer"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pt(max(bound),df,lower.tail = FALSE) + pt(min(bound),df,lower.tail = TRUE)
    }
    # Inverse
    if(inverse == TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qt(bound,df,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## This piece is for calculation on the Chi-Squared distribution.
  if(type == "chi-squared"){
    #one tailed
    if(tail == "left" | tail == "right"){
      probability <- pchisq(bound,df,lower.tail = logic)
    }
    #interior
    if(tail == "inner"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pchisq(max(bound),df,lower.tail = TRUE) - pchisq(min(bound),df,lower.tail = TRUE)
    }
    #two tailed
    if(tail == "two" | tail == "outer"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pchisq(max(bound),df,lower.tail = FALSE) + pchisq(min(bound),df,lower.tail = TRUE)
    }
    # Inverse
    if(inverse == TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qchisq(bound,df,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## This is for the Binomial
  if(type == "binomial"){
    #one tailed
    if(tail == "left" | tail == "right"){
      probability <- pbinom(bound,trials,prob,lower.tail = logic)
    }
    #interior
    if(tail == "inner"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pbinom(max(bound),trials,prob,lower.tail = TRUE) - pbinom(min(bound),trials,prob,lower.tail = TRUE)
    }
    #two tailed
    if(tail == "two" | tail == "outer"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pbinom(max(bound),trials,prob,lower.tail = FALSE) + pbinom(min(bound),trials,prob,lower.tail = TRUE)
    }
    # Inverse
    if(inverse == TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qbinom(bound,trials,prob,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## Return statement
  if(inverse == FALSE){
    return(probability)
  }else{
    return(value)
  }
}


