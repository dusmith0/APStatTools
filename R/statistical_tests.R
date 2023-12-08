
#' Title t_test.one   One sample t-test for means.
#'
#' @param nullInput Numeric: This is a single value for the Null Hypothesis's expected value.
#' @param x_bar Input Vector: This is the value of the sample means. It assumes the standard error adjustment has not been taken.
#' @param sigma Input Vector: This is the value of the sample standard deviations.
#' @param n Input Vector: This is the value of the sample sizes.
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain Test Name, p_value, test_statistic, n, standard_error, df,
#' @export
#'
#' @examples
#' t_test.one(null = 12, x_bar = 10, sigma = 5, n = 25, tail = "left", graph = TRUE)
#'
t_test.one <- function(null,x_bar,sigma,n,tail="left",graph=TRUE){
  # calculates needed values
  df <- n - 1
  standard_error <- sigma / sqrt(n)
  test_statistic <- (x_bar - null) / standard_error
  #runs the p_value through the normal cdf based on value of tail.
  if(tail == "left"){
    p_value <- pt(test_statistic,df,lower.tail = TRUE)
  }else if(tail == "right"){
    p_value <- pt(test_statistic,df,lower.tail = FALSE)
  }else{p_value <- pt(test_statistic,df,lower.tail = TRUE) * 2}
  #prints a flagged distribution
  if(graph == TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)
  }

  return(data.frame(test = "One Sample t-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
}

## Two sample t-test on means
#' Title
#'
#' @param nullInput Numeric: This is a single value for the Null Hypothesis's expected value.
#' @param x_bar Input Vector of two: This is the value of the sample means c(left, right). It assumes the standard error adjustment has not been taken.
#' @param sigma Input Vector of two: This is the value of the sample standard deviations c(left, right).
#' @param n Input Vector f two: This is the value of the sample sizes c(left, right).
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain Test Name, p_value, test_statistic, n, standard_error, df,
#' @export
#'
#' @examples
#' t_test.two(null = 0, x_bar = c(11,10), sigma = c(4,5), n = c(30,25), tail = "left", graph = TRUE)
#'
#'
t_test.two <- function(null = 0,x_bar,sigma,n,tail="left",graph=TRUE){
  # checking input for null
  if(length(null) != 1){
    null <- diff(-null)
  }
  # calculates error and statistic
  df <- ((sigma[1] ^ 2 / n[1] + sigma[2] ^ 2 / n[2]) ^ 2) / sum((1 / (n - 1)) * (sigma ^ 2 / n) ^ 2)
  standard_error <- c(sigma[1]/sqrt(n[1]), sigma[2]/sqrt(n[2]))
  standard_error_combined <- sqrt( sigma[1] ^ 2 / n[1] + sigma[2] ^ 2 / n[2])
  test_statistic <- (diff(-x_bar) - null) / standard_error_combined
  #runs the p_value through the normal cdf based on value of tail.
  if(tail == "left"){
    p_value <- pt(test_statistic,df,lower.tail = TRUE)
  }else if(tail == "right"){
    p_value <- pt(test_statistic,df,lower.tail = FALSE)
  }else{p_value <- pt(test_statistic,df,lower.tail = TRUE) * 2}
  #prints a flagged distribution
  if(graph == TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)
  }
  return(data.frame(test = "Two Sample t-test", p_value = p_value, test_statistic = test_statistic,
                    n = n, standard_error = standard_error, standard_error_combined = standard_error_combined, df = df))
}

## Pooled two sample t-test
## Note: This will assume the expected difference is calculated as table_one - table_two.
## Please input the data in the order you would like them subtracted, to match the above.
#' Title
#'
#' @param null Input Numeric: This is a single value for the Null Hypothesis's expected value.
#' @param table_one Input Vector: This is a vector of numeric values intended for the
#'         t_test.paired test only. This is the first vector to be compared. The function will assume table_one - table_two.
#' @param table_two Input Vector: This is a vector of numeric values intended for the
#'         t_test.paired test only. This is the second vector to be compared. The function will assume table_one - table_two.
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain Test Name, p_value, test_statistic, n, standard_error, df,
#' @export
#'
#' @examples
#' X <- c(10,11,12,13,14,15,16,17,18,19,20)
#' Y <- c(12,11,13,14,14,9,15,13,20,14,18)
#' t_test.paired(null = 0, table_one = X, table_two = Y, tail = "left", graph = TRUE)
#'
t_test.paired <- function(null = 0,table_one,table_two,tail="two",graph=TRUE){
  # checking input for null
  if(length(null) != 1){
    null <- diff(-null)
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
  if(graph == TRUE){
    build_dist(type = "t-dist", tail = tail, bound = test_statistic, df = df)
  }
  return(data.frame(test = "Two Sample Paired t-test", paired_data = paired_data, p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error, df = df))
}


##-------------------------------------------------------------------------##
## z-test family
## Note: This test will fail if null = 0
#' Title
#'
#' @param null Input Numeric: This is a single value for the Null Hypothesis's expected value.
#' @param p_hat Input Vector: This is the value of the sample proportions.
#'         It assumes the standard error adjustment has not been taken, and that the value is in decimal form.
#' @param n Input Vector: This is the value of the sample sizes.
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used. Be aware that the function will also assume a "left" tail when preforming
#'         the two tailed calculation. To avoid this, set tail to "right" and multiply by 2.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain some of the following values, given the appropriate test.
#'         Test Name, p_value, test_statistic, n, standard_error.
#' @export
#'
#' @examples
#'
#' z_test.one(null = .4, p_hat = .6, n = 10, tail = "left", graph = TRUE)
#'
#'
z_test.one <- function(null,p_hat,n,tail="left",graph=TRUE){
  if(p_hat >= 1){
    stop(paste("Error: This function requires that the p_hat is a decimal number."))
  }
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
  if(graph == TRUE){
    build_dist(type = "normal", tail = tail, bound = test_statistic)
  }

  return(data.frame(test = "One Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
}

## Two sample test on proportions.
#' Title
#'
#' @param p_hat Input Vector of two: This is the value of the sample proportions c(left,right)
#'         It assumes the standard error adjustment has not been taken, and that the value is in decimal form.
#' @param n Input Vector of two: This is the value of the sample sizes  c(left,right).
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used. Be aware that the function will also assume a "left" tail when preforming
#'         the two tailed calculation. To avoid this, set tail to "right" and multiply by 2.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain some of the following values, given the appropriate test.
#'         Test Name, p_value, test_statistic, n, standard_error.
#' @export
#'
#' @examples
z_test.pooled <- function(p_hat,n,tail="left",graph=TRUE){
  if(any(p_hat >= 1)){
    stop(paste("Error: This function requires that the p_hat is a decimal number."))
  }
  #finding standard error.
  pc <- sum(p_hat * n) / sum(n)
  standard_error <- sqrt(pc * (1 - pc) * (sum(1 / n)))
  test_statistic <- (p_hat[1] - p_hat[2]) / standard_error

  #runs the p_value through the normal cdf based on value of tail.
  if(tail == "left"){
    p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE)
  }else if(tail == "right"){
    p_value <- pnorm(test_statistic,0,1,lower.tail = FALSE)
  }else{p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE) * 2}
  #prints a flagged distribution
  if(graph == TRUE){
    build_dist(type = "normal", tail = tail, bound = test_statistic)
  }
  return(data.frame(test = "Two Sample z-test", p_value = p_value, test_statistic = test_statistic, n = n, standard_error = standard_error))
}


##--------------------------------------------------------------------------##
## Chi-squared family
## Please note that the base::chisq.test() is much more robust to this function. The blow
## is only intended to mimic the question style of stimulus used in AP Statistics.
## It is not intended as a replacement to the base function.
#' Title
#'
#' @param obs_table
#' @param expected_table
#' @param expected_as_count
#' @param row_totals
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
chi_squared.gof <- function(obs_table, expected_table = NULL, expected_as_count = FALSE, row_totals = FALSE, graph=TRUE){
  ## Checking some conditions.
  if(row_totals == TRUE){
    obs_table <- obs_table[-length(obs_table)]
    expected_table <- expected_table[-length(expected_table)]
  }


  if(length(obs_table) != length(expected_table)){
    stop(paste("Error: Please ensure your table lenghts are equal in size. Also this function runs assuming that you did not
                 input the 'Total' column into your obs_table. If you did please add
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
    expected_count <- sum(obs_table) * expected_table
  }else{
    expected_count <- expected_table
  }

  ##Here we compute the actual calculation.
  df <- length(obs_table) - 1
  chi_squared_values <- (( expected_count - obs_table) ^ 2) / expected_count
  test_statistic <- sum(chi_squared_values)
  p_value <- pchisq(test_statistic,df,lower.tail = FALSE)

  ## This piece builds the graphic.
  if(graph == TRUE){
    build_dist(type="chi-squared",tail="right",test_statistic,df,prob=FALSE)
  }
  return(data.frame(test = "Pearson's Chi-Squared GOF Test", obs_table = obs_table, expected_count = expected_count, chi_squared_values = chi_squared_values, df = df, test_statistic = test_statistic, p_value = p_value))
}


##Chi-Squared Test for Homogeneity or Independence.
#' Title
#'
#' @param obs_table
#' @param mat_totals
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
chi_squared.ind <- function(obs_table, mat_totals = FALSE,graph=TRUE){
  #Generating totals if not provided
  if(mat_totals == FALSE){
    obs_table <- rbind(obs_table,colSums(obs_table))
    obs_table <- cbind(obs_table,rowSums(obs_table))
  }

  #Generating the expected tables
  expected <- matrix(0,nrow=nrow(obs_table), ncol=ncol(obs_table))
  for(i in 1:nrow(obs_table)){
    for(j in 1:ncol(obs_table)){
      expected[i,j] <- obs_table[i,ncol(obs_table)] * obs_table[nrow(obs_table),j]
    }
  }
  expected_count <- expected / obs_table[nrow(obs_table),ncol(obs_table)]

  # Calculating the Chi-Squared Statistic
  exp_count <- expected_count[-nrow(expected_count),-ncol(expected_count)]
  null <- obs_table[-nrow(obs_table),-ncol(obs_table)]
  df <- (ncol(null) - 1) * (nrow(null) - 1)

  chi_squared_values <- ((exp_count-null) ^ 2) / exp_count
  test_statistic <- sum(chi_squared_values)
  p_value <- pchisq(test_statistic,df,lower.tail = FALSE)

  ## This piece builds the graphic.
  if(graph == TRUE){
    build_dist(type="chi-squared",tail="right",test_statistic,df,prob=FALSE)
  }
  return(list(test = "Pearson's Chi-Squared GOF Test", obs_table = obs_table, expected_count = expected_count, chi_squared_values = chi_squared_values, df = df, test_statistic = test_statistic, p_value = p_value))

}


