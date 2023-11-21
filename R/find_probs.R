
find_probs <- function(bound, type="normal", tail="left", mean = 0, sd = 1, df = 1, prob = .5, trials = 10, inverse = FALSE){
  ## reading in the bounds for the calculations
  if(tail == "left"){
    logic <- TRUE
  }else{
    logic <- FALSE
  }

  ## Calculations on the Normal
  if(type = "normal"){
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
    if(inverse = TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qnorm(bound,mean,sd,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## Calculations on the t-distribution
  if(type = "t-dist"){
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
    if(inverse = TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qt(bound,df,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## This piece is for calculation on the Chi-Squared distribution.
  if(type = "chi-squared"){
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
    if(inverse = TRUE){
      if(bound > 1){
        bound <- bound/100
      }
      value <- qchisq(bound,df,lower.tail = TRUE)
      warning("This function finds percentile values (the area to the left) only.")
    }
  }

  ## This is for the Binomial
  if(type = "binomial"){
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
    if(inverse = TRUE){
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
