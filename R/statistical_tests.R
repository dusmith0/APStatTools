##-------------------------------------------------------------------------##
## t_test family of functions
## One sample t-test for means.
t_test_one <- function(null,x_bar,sd,n,tail="left",graph=TRUE){
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
  if(graph = TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)
  }

  return(data.frame(test = "One Sample t-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
}

## Two sample t-test on means
t_test_two <- function(null = 0,x_bar,sd,n,tail="left",graph=TRUE){
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
  if(graph = TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)
  }
  return(data.frame(test = "Two Sample t-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
}

## Pooled two sample t-test
## Note: This will assume the expected difference is calculated as table_one - table_two.
## Please input the data in the order you would like them subtracted, to match the above.
t_test_paired <- function(null = 0,table_one,table_two,tail="two",graph=TRUE){
  # checking input for null
  if(length(null) != 1){
    null <- diff(null)
  }

  if(length(table_one) != length(table_two)){
    stop(paste("Error: Please ensure that your input of your data sets match in size."))
  }

  # calculates error and statistic
  paired_data <- table_one - table_two
  n <- length(paired_data)
  df <- n - 1
  x_bar <- mean(paired_data)
  standard_error <- sd(paired_data)/sqrt(n)

  test_statistic <- (x_bar - null) / standard_error
  #runs the p_value through the normal cdf based on value of tail.
  if(tail == "left"){
    p_value <- pt(test_statistic,df,lower.tail = TRUE)
  }else if(tail == "right"){
    p_value <- pt(test_statistic,df,lower.tail = FALSE)
  }else if(test_statistic > 0){
    p_value <- pt(test_statistic,df,lower.tail = FALSE) * 2
  }else if(test_statistics <= 0){
    p_value <- pt(test_statistic,df,lower.tail = TRUE) * 2
  }
  #prints a flagged distribution
  if(graph = TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df,graph=TRUE)
  }
  return(data.frame(test = "Two Sample Paired t-test", paired_data = paired_data, p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
}



}
##-------------------------------------------------------------------------##
## z-test family
z_test_one <- function(null,p_hat,n,tail="left",graph=TRUE){
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
  if(graph = TRUE){
    build_dist(type = "normal", tail = tail, bound = test_statistic)
  }

  return(data.frame(test = "One Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
}

## Two sample test on proportions.
z_test_pooled <- function(p_hat,n,tail="left",graph=TRUE){
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
  if(graph = TRUE){
    build_dist(type = "normal", tail = tail, bound = test_statistic)
  }
  return(data.frame(test = "Two Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
}


##--------------------------------------------------------------------------##
## Chi-squared family
## Please note that the base::chisq.test() is much more robust to this function. The blow
## is only intended to mimic the question stlye of stimulous used in AP Statistics.
## It is not intended as a replacement to the base function.
chi_squared_gof <- function(null_table, expected_table = NULL, expected_as_count = FALSE, row_totals = FALSE,graph=TRUE){
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
  if(graph = TRUE){
    build_dist(type="chi-squared",tail="right",test_statistic,df,prob=FALSE)
  }
  return(list(test = "Pearson's Chi-Squared GOF Test", null_table = null_table, expected_count = expected_count, chi_squared_values = chi_squared_values, df = df, test_statistic = test_statistic, p_value = p_value))
}


##Chi-Squared Test for Homogenaity or Independence.
chi_squared_independence <- function(null_table, mat_totals = FALSE,graph=TRUE){
  #Generating totals if not provided
  if(mat_totals == FALSE){
    null_table <- rbind(null_table,colSums(null_table))
    null_table <- cbind(null_table,rowSums(null_table))
  }

  #Generating the expected tables
  expected <- matrix(0,nrow=nrow(null_table), ncol=ncol(null_table))
  for(i in 1:nrow(null_table)){
    for(j in 1:ncol(null_table)){
      expected[i,j] <- null_table[i,ncol(null_table)] * null_table[nrow(null_table),j]
    }
  }
  expected_count <- expected / null_table[nrow(null_table),ncol(null_table)]

  # Calculating the Chi-Squared Statistic
  exp_count <- expected_count[-nrow(expected_count),-ncol(expected_count)]
  null <- null_table[-nrow(null_table),-ncol(null_table)]
  df <- (ncol(null) - 1) * (nrow(null) - 1)

  chi_squared_values <- ((exp_count-null) ^ 2) / exp_count
  test_statistic <- sum(chi_squared_values)
  p_value <- pchisq(test_statistic,df,lower.tail = FALSE)

  ## This piece builds the graphic.
  if(graph = TRUE){
    build_dist(type="chi-squared",tail="right",test_statistic,df,prob=FALSE)
  }
  return(list(test = "Pearson's Chi-Squared GOF Test", null_table = null_table, expected_count = expected_count, chi_squared_values = chi_squared_values, df = df, test_statistic = test_statistic, p_value = p_value))

}
