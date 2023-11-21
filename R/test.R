#This needs to be a large wrapper function.

test <- function(null,x_bar,p_hat,sd,n,df,test,tail="left",data=NULL,graph=TRUE){
  ## Preform some error checks for correct input.


  ##Check to see if the information was input as data,
  ##if so find the mean and sd based on number of columns in the data.
  if(!is.null(data)){

  }

  #maybe define the norm calculations here?

  ##Place a call function here

  ##-------------------------------------------------------------------------##
  #Functions for tests go here-----------------------------------------------##

  ## t_test family of functions
  ## One sample t-test for means.
  t_test_one <- function(null,x_bar,sd,n,tail="left"){
    # calculates needed values
    df <- n - 1
    standard_error <- sd / sqrt(n)
    test_statistic <- (x_bar - null) / standard_error
    #runs the p_value through the normal cdf based on value of tail.
    if(tail == "left"){
      p_value <- pt(test_statistic,df,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pt(test_statistic,df,lower.tail = FALSE)
    }else{p_value <- pt(test_statistic,df,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)

    return(data.frame(test = "One Sample t-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
  }

  ## Two sample z-test on means
  t_test_two <- function(null,x_bar,sd,n,tail="left"){
    # checking input for null
    if(length(null) != 1){
      null <- diff(null)
    }
    # calculates error and statistic
    df <- ((sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2]) ^ 2) / sum((1 / (n - 1)) * (sd ^ 2 / n) ^ 2)
    standard_error <- sqrt( sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2])
    test_statistic <- (diff(x_bar) - null) / standard_error
    #runs the p_value through the normal cdf based on value of tail.
    if(tail == "left"){
      p_value <- pt(test_statistic,df,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pt(test_statistic,df,lower.tail = FALSE)
    }else{p_value <- pt(test_statistic,df,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)

    return(data.frame(test = "Two Sample t-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
  }

  ## Pooled two sample t-test
  t_test_pooled <- function(){



  }

  ## z-test family
  z_test_one <- function(null,p_hat,n,tail="left"){
    #finding standard error.
    standard_error <- sqrt(null * (1 - null) / n)
    test_statistic <- (p_hat - null) / standard_error

    #runs the p_value through the normal cdf based on value of tail.
    if(tail == "left"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = FALSE)
    }else{p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "normal", tail = tail, bound = test_statistic)

    return(data.frame(test = "One Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
  }

  ## Two sample test on proportions.
  z_test_pooled <- function(p_hat,n,tail="left"){
    #finding standard error.
    pc <- sum(p_hat * n) / sum(n)
    standard_error <- sqrt(pc * (1 - pc) * (sum(1 / n)))
    test_statistic <- (diff(p_hat)) / standard_error

    #runs the p_value through the normal cdf based on value of tail.
    if(tail == "left"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = FALSE)
    }else{p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "normal", tail = tail, bound = test_statistic)

    return(data.frame(test = "Two Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
  }

  ## Chi-squared family
  ## Please note that the base::chisq.test() is much more robust to this function. The blow
  ## is only intended to mimic the question stlye of stimulous used in AP Statistics.
  ## It is not intended as a replacement to the base function.
  chi_squared_gof <- function(null_table, expected_table = NULL, expected_as_count = FALSE, row_totals = FALSE){
    ## Cheching some conditions.
    if(row_totals == TRUE){
      null_table <- null_table[-length(null_table)]
    }

    if(length(null_table) != length(expected_table)){
      stop(paste("Error: Please ensure your table lenghts are equal in size. Also this function runs assuming that you did not
                 input the 'Total' column into your null_table. If you did please add
                 row_tatals == TRUE to your input."))
    }

    if(is.null(expected_table)){
      stop(paste("Error: This functions assumes that you have a known expected frequency"))
    }

    if(any(expected_table > 1) & expected_as_count == FALSE){
      expected_table <- expected_table / 100
      message("It seems that one of your expected values is greater then one. They have been adjusted to decimal values.
              \n If they were intended to be expected counts not percentages please recall the function and add
              \n expected_as_count = TRUE
              \n to your input.")
    }

    ##This piece reads in the expected_table and generates a count for calculation.
    if(expected_as_count == FALSE){
      expected_count <- sum(null_table) * expected_table
    }else{
      expected_count <- expected_table
    }

    ##Here we compute the actual calculation.
    df <- length(null_table) - 1
    chi_squared_values <- (( expected_count - null_table) ^ 2) / expected_count
    test_statistic <- sum(chi_squared_values)
    p_value <- pchisq(test_statistic,df,lower.tail = FALSE)

    ## This piece builds the graphic.
    build.dist(type="chi-squared",tail="right",test_statistic,df,prob=FALSE)

    return(list(test = "Pearson's Chi-Squared GOF Test", null_table = null_table, expected_count = expected_count, df = df, test_statistic = test_statistic, p_value = p_value))
  }






  return(list(test = "Two Sample z-test", p_value = p_value, test_statistic = test_statistic, x_bar = x_bar, se = se, n = n, df = df))
}


tests <- list("t-test","t-test.paired","z-test.one","z-test.two","z-test.pooled","chi-squared","ANOVA",
              "chi-squared.GOF","chi-squared.homogeneity","chi-squared.independence")

tail <- list("left","right","two")
