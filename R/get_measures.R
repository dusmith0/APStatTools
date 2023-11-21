## A set of functions to find general values from a list or lists of data.
get_measures <- function(input = FALSE, X = NULL, Y = NULL, freq = c(1), frequency = FALSE, regression = FALSE, outliers = NULL){
  ## A piece to allow for user input for data, and reading it into different columns
  if(input == TRUE){
    data <- data.frame()
    data <- edit(data)
    data <- as.matrix(data)
  }

  if(input == FALSE){
    data <- cbind(X,Y)
  }

  ## A piece to check if frequency or regression is intended or not intended

  ## Setting data types for quantile values
  Q1 <- rep(0,ncol(data))
  Q2 <- rep(0,ncol(data))
  Q3 <- rep(0,ncol(data))
  IQR <- rep(0,ncol(data))
  lower_outliers <- rep(0,ncol(data))
  upper_outliers <- rep(0,ncol(data))

  ## A peice to calculate all types of measures
    ## List(n, pop_mean, sample_mean, pop_sd, sample_sd, , Q1, Q2, Q3, IQR, r, r-squared, a, b for LSRL)
      totals <- colSums(data)
      n <- nrow(data)
      mean <- colMeans(data)
      pop_sd <- apply(data,2,function(z) sd(z))
      sample_sd <- apply(data,2,function(z) sd(z))
      for(i in 1:ncol(data)){
        Q1[i] <- mean(data[floor((n + 1) / 4),i], data[ceiling((n + 1) / 4),i])
        Q2[i] <- mean(data[floor((n + 1) / 2),i], data[ceiling((n + 1) / 2),i])
        Q3[i] <- mean(data[floor(3 * (n + 1) / 4),i], data[ceiling(3 * (n + 1) / 4),i])
      }
      IQR <- Q3 - Q1

      if(!is.null(outliers)){
        if(outliers == "1.5 IQR"){
          lower_outliers <- Q1 - 1.5 * IQR
          upper_outliers <- Q3 + 1.5 * IQR
        }else if(outliers == "3sd Rule"){
          lower_outliers <- mean - (3 * sample_sd)
          upper_outliers <- mean - (3 * sample_sd)
        }
      }

      if(regression == TRUE){
        LSRL <- summary(lm(data[,2]~data[,1]))
      }

      if(regression == FALSE & outliers == FALSE){
        return(list(data = data, totals = totals, n = n, mean = mean, pop_sd = pop_sd, sample_sd = sample_sd, Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR))
      }

      if(regression == TRUE & outliers == FALSE){
        return(list(data = data, totals = totals, n = n, mean = mean, pop_sd = pop_sd, sample_sd = sample_sd, Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR, LSRL = LSRL))
      }

      if(regression == FALSE & outliers == TRUE){
        return(list(data = data, totals = totals, n = n, mean = mean, pop_sd = pop_sd, sample_sd = sample_sd, Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR, lower_outliers = lower_outliers, upper_outliers = upper_outliers))
      }

      if(regression == TRUE & outliers == TRUE){
        return(list(data = data, totals = totals, n = n, mean = mean, pop_sd = pop_sd, sample_sd = sample_sd, Q1 = Q1, Q2 = Q2, Q3 = Q3, IQR = IQR, lower_outliers = lower_outliers, upper_outliers = upper_outliers, LSRL = LSRL))
      }
}
