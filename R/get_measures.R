## A set of functions to find general values from a list or lists of data.
get_measures <- function(input = FALSE, X = NULL, Y = NULL, freq = c(1), frequency = FALSE, regression = FALSE, outliers = FALSE){
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
  measures <- data.frame(colnames(data))

  ## A peice to calculate all types of measures
    ## List(n, pop_mean, sample_mean, pop_sd, sample_sd, , Q1, Q2, Q3, IQR, r, r-squared, a, b for LSRL)
      measures[,2] <- totals <- colSums(data)
      measures[,3] <- n <- nrow(data)
      measures[,4] <- mean <- colMeans(data)
      measures[,5] <- pop_sd <- apply(data,2,function(z) sd(z))
      measures[,6] <- sample_sd <- apply(data,2,function(z) sd(z))
      for(i in 1:ncol(data)){
        Q1[i] <- mean(data[floor((n + 1) / 4),i], data[ceiling((n + 1) / 4),i])
        Q2[i] <- mean(data[floor((n + 1) / 2),i], data[ceiling((n + 1) / 2),i])
        Q3[i] <- mean(data[floor(3 * (n + 1) / 4),i], data[ceiling(3 * (n + 1) / 4),i])
      }
      measures[,7] <- Q1
      measures[,8] <- Q2
      measures[,9] <- Q3
      measures[,10] <- IQR <- Q3 - Q1

      names(measures) <- c("data","totals","n","mean","pop_sd","sample_sd","Q1","Q2","Q3","IQR")

      if(outliers != FALSE){
        if(outliers == "1.5 IQR"){
          lower_outliers <- Q1 - 1.5 * IQR
          upper_outliers <- Q3 + 1.5 * IQR
        }else if(outliers == "3sd Rule"){
          lower_outliers <- mean - (3 * sample_sd)
          upper_outliers <- mean - (3 * sample_sd)
        }
        measures[,11] <- lower_outliers
        measures[,12] <- upper_outliers
        names(measures)[11] <- "lower_outliers"
        names(measures)[12] <- "upper_outliers"
      }


      if(regression == TRUE){
        LSRL <- summary(lm(data[,2]~data[,1]))
      }

      if(regression == FALSE){
        return(list(data = data, measures = measures))
      }

      if(regression == TRUE & outliers == TRUE){
        return(list(data = data, measures = measures, LSRL = LSRL))
      }
}
