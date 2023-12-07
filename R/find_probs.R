
#' Title find_probs()
#'
#' @Description: This is a simple wrapper function for finding the probabilities for various distributions.
#'         Notes One: This function is not indented to explicitly push out p-values. You will need to standardize and find
#'               the appropriate Standard Errors before hand. (As in two-sided hypothesis tests will not be multiplied by 2.)
#'               Two: If you wish for the a graphic of the distribution try build_dist(...display_prob=TRUE)
#' @param bound Input Vector of length one or two: c(left,right).
#'        This is to define the boundaries of the shaded region of the graph.
#'        Notes: "inner" and "two" require both the left and right input.
#'        Notes: The default of NULL will always produce an error. (This is intentional)
#' @param type Input String being any of "normal", "t-dist", "chi-squared", "binomial"
#' @param tail Input String being any of "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal"
#'        Notes: "left_not_equal" and "left_not_equal" are intended for the "binomial" only.
#'        Notes: "out" and "two" will produce the same output.
#' @param mean Input Numeric: Default = 0
#'        Vector of one value. The mean of the distribution
#' @param sigma Input Numeric: Default = 1
#'        Vector of one value. The standard deviation of a distribution.
#' @param df Input Numeric: Default = 1
#'        Vector of one value. The degree of freedom of a distribution.
#' @param prob Input Numeric: Default = .5
#'        Vector of one value. The probability of success in the binomial distribution.
#' @param trials Input Numeric: Default = 10
#'        Vector of one value. The number of trials in the binomial distribution.
#' @param inverse Input Logical: Default = FALSE
#'        If true the function will assume bound is a Area, and the function will attempt to find the
#'        quantile that produces a probability of "bound"
#'        Note: This function assumes only "left" as a tail. Imputing other tails will have no effect on the output.
#'
#' @return Numeric: Probability = being the calculated probability of the region supplied. (The cdf)
#'         Numeric: Value = being the calculated value that produces the area supplied in bound. (the quantile)
#'
#' @export
#'
#' @examples
#' # Will assume Normal
#' find_probs(1.5)
#'
#' # For multiple outputs for the Normal
#' find_probs(c(-3,-2,-1,0,1,2,3))
#'
#'
#' # Probability between two points of a chi-squared
#' find_probs(c(-1,3),type="chi-squared",tail="inner",df=4)
#'
#' # Using a t-distribution
#' find_probs(c(-1,3),type="t-dist",tail="right",df=4)
#'
#' # To find the outside tailed probabilities
#' find_probs(c(2,6),type="binomial",tail="two")
#' find_probs(c(2,6),type="binomial",tail="outer")
#' find_probs(4,type="binomial",tail="left")
#' find_probs(4,type="binomial",tail="left_not_equal")
#'
#' # Finding inverses
#' find_probs(bound = .4, inverse = TRUE)
#' find_probs(bound = .8, mean = 10, sigma = 4, inverse = TRUE)
#'
find_probs <- function(bound = NULL, type="normal", tail="left", mean = 0, sigma = 1, df = 1, prob = .5, trials = 10, inverse = FALSE){
  if(is.null(bound)){
    stop(paste("Error: Please enter a value for bound."))
  }
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
      probability <- pnorm(bound,mean,sigma,lower.tail = logic)
    }
    #interior
    if(tail == "inner"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pnorm(max(bound),mean,sigma,lower.tail = TRUE) - pnorm(min(bound),mean,sigma,lower.tail = TRUE)
    }
    #two tailed
    if(tail == "two" | tail == "outer"){
      if(length(bound) != 2){
        stop(paste("Error: Please ensure that you enter and upper and lower bound when you use the 'inner' tails."))
      }
      probability <- pnorm(max(bound),mean,sigma,lower.tail = FALSE) + pnorm(min(bound),mean,sigma,lower.tail = TRUE)
    }
    # Inverse
    if(inverse == TRUE){
      if(bound > 1){
        bound <- bound/100
        warning(paste("Your bound was great then one. Thus a Percentage input was assumed and ",bound,"was used for the calcuation."))
      }
      value <- qnorm(bound,mean,sigma,lower.tail = TRUE)
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
        warning(paste("Your bound was great then one. Thus a Percentage input was assumed and ",bound,"was used for the calcuation."))
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
        warning(paste("Your bound was great then one. Thus a Percentage input was assumed and ",bound,"was used for the calcuation."))
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
    if(tail == "left_not_equal"){
      probability <- pbinom(bound-1,trials,prob,lower.tail = TRUE)
    }
    if(tail == "right_not_equal"){
      probability <- pbinom(bound+1,trials,prob,lower.tail = FALSE)
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
        warning(paste("Your bound was great then one. Thus a Percentage input was assumed and ",bound,"was used for the calcuation."))
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


