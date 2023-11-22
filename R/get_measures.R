#' Title get_measures()
#'
#' Description: This function will take user input data or data sets and compute a large number of statistical values.
#'
#'
#' @param input logical. If TRUE it will prompt the user to input their data into a spreadsheet.
#'              If FALSE the function will expect the user to input the data into X and/or Y.
#' @param X Vector of numeric data. This is not intended to be used for a Matrix. Please use input = TRUE to input a matrix.
#' @param Y Vector of numeric data
#' @param freq
#' @param frequency
#' @param regression logical. IF TRUE the function will print out the summary(lm) for Y ~ X.
#' @param outliers String being any of "FALSE", "1.5 IQR", "3sd Rule".
#'                 If FALSE the function will not print bounds for outliers. If a rule is implemented the function will
#'                 print a lower and upper boundary for outliers of the given rule.
#'
#' @return list($data, $measures, $LSRL)
#'         data: Matrix that contains the data the user input.
#'         measures: is a data.frame() containing statistical measures about the users data.
#'            measures can contain any of list("data","totals","n","mean","pop_sd","sample_sd","Q1","Q2","Q3","IQR","lower_outliers","upper_outliers")
#'          LSRL: the list created by summary(lm()) in base R
#' @export
#'
#' @examples
get_measures <- function(input = FALSE, X = NULL, Y = NULL, freq = c(1), frequency = FALSE, regression = FALSE, outliers = FALSE){
  ## A piece to allow for user input for data, and reading it into different columns
  if(input == TRUE){
    data <- data.frame()
    data <- edit(data)
    data <- as.matrix(data)
  }

  if(input == FALSE){
    if(!is.null(X) & !is.null(Y)){
      if(length(X) != length(Y)){
        stop(paste("Error: Please ensure that your input values are of equal lenght."))
      }
      data <- cbind(X,Y)
      data <- as.data.frame(data)
    }else if(!is.null(X)){
      data <- X
    }else if(!is.null(Y)){
      data <- Y
    }
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
