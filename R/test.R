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

  ## z_test family of functions
  ## One sample z-test for means.
  z.test.one <- function(null,x_bar,sd,n,tail="left"){
    # calculates needed values
    df <- n - 1
    standard_error <- sd / sqrt(n)
    test_statistic <- (x_bar - null) / standard_error
    #runs the p_value through the normal cdf based on value of tail.
    if(tail == "left"){
      p_value <- pt(test_statistic,df,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pt(test_statistic,df,lower.tail = FALSE)
    }else{p_value <- pf(test_statistic,df,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)

    return(data.frame(test = "One Sample Z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
  }

  ## Two sample z-test on means
  z.test.two <- function(null,x_bar,sd,n,tail="left"){
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
    }else{p_value <- pf(test_statistic,df,lower.tail = TRUE) * 2}
    #prints a flagged distribution
    build.dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)

    return(data.frame(test = "Two Sample Z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
  }

  ## Pooled two sample t-test
  z.test.pooled <- function(){

  }

  ## t-test family
  t.test.one <- function(){

  }

  t.test.two <- function(){

  }

  ## Chi-squared family


  return(list(p_value = p_value, test_statistic = test_statistic, x_bar = x_bar, se = se, n = n, df = df))
}


tests <- list("t-test","t-test.paired","z-test.one","z-test.two","z-test.pooled","chi-squared","ANOVA",
              "chi-squared.GOF","chi-squared.homogeneity","chi-squared.independence")

tail <- list("left","right","two")
