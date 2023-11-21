## A set of functions to find general values from a list or lists of data.
get_measures <- function(input = FALSE, data_one, data_two, data_three, data_four, frequency = FALSE, regression = FALSE){
  ## A piece to allow for user input for data, and reading it into different columns
  if(input == TRUE){
    data <- data.frame()
    data <- edit(data)
    data <- as.matrix(data)
  }
  ## A peice to check if frequency or regressoin is intended or not intended

  ## A peice to calculate all types of measures
    ## List(n, pop_mean, sample_mean, pop_sd, sample_sd, , Q1, Q2, Q3, IQR, r, r-squared, a, b for LSRL)








}
