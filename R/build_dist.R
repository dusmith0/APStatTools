#' Title build_dist
#'
#' Description: This function is designed to build a set of distributions and shade in
#'               the various regions of interest.
#'
#'               Note: This builds only standardized distributions. It assumes the mean=0 and sd=1 in many situations.
#'               Note2: The graphics provided for the binomial distribution, while correct, may not provide pleasing images for
#'                      large trials. (that is much more than 1000)
#'
#' @param type string being any of "normal", "t-dist", "chi-squared", "binomial", "QQ", "regression"
#' @param tail string being any of "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal"
#'        Notes: "left_not_equal","left_not_equal","inner_not_equal", and "two_not_equal" are intended for the "binomial" only.
#'        Notes: "out" and "two" will produce the same output.
#' @param bound vector of length one or two: c(left,right). This is to define the boundaries of the shaded region of the graph.
#'        Notes: "inner" and "two" require both the left and right input.
#' @param df numeric single value for the degree of freedom
#' @param prob numeric value for the probability of success. Imputing values of greater than 1 may cause errors.
#' @param trials numeric value for the number of trials that will occur for the "binomial"
#' @param display_prob logical If true this will print the probability of the shaded region along with the graphic.
#'        If the type = "regression" setting display_prob = TRUE will call get_measures() on your data.
#' @param X numeric vector Specifically for type = "regression"
#' @param Y numeric vector Specifically for type = "regression"
#' @param data numeric matrix that can be used instead of imputing X and Y for regression.
#'
#' @return Graph using the plot() function, along with an optional numeric vector of one.
#' @export
#'
#' @examples
#' # For the Standard Normal
#' build_dist()
#'
#' # For t-distribution with a degree of freedom of 1
#' build_dist(type = "t-dist")
#'
#' # For a binomial distribution
#' build_dist(type = "binomial")
#'
#' # Shading different sections
#' build_dist(type = "normal", bound = -1)
#' build_dist(type = "normal", bound = c(-1,2), tail = "inner")
#' build_dist(type = "t-dist", bound = c(-1,0), tail = "two")
#' build_dist(type = "binomial",bound = c(6,12),
#'    tail = "outer", p = .5, trials = 20)
#'
#' # adding probabilities to the graphics
#' build_dist(type = "normal", bound = -1, display_prob = TRUE)
#' build_dist(type = "normal", bound = c(-1,2),
#'    tail = "inner", display_prob = TRUE)
#' build_dist(type = "t-dist", bound = c(-1,0),
#'    tail = "two", display_prob = TRUE)
#' build_dist(type = "binomial",bound = c(6,12),
#'    tail = "outer", p = .5, trials = 20, display_prob = TRUE)
#'
#' # Regression
#' X <- rnorm(25,0,1)
#' Y <- sample(1:10,25,replace = TRUE)
#' build_dist(type = "regression", X = X, Y = Y)
#' build_dist(type = "regression", X = X, Y = Y, display_prob = TRUE)
#'
#' # QQ plots
#' build_dist(type = "QQ", data = X)
#' build_dist(type = "QQ", data = Y)
#'
build_dist <- function(type="normal", tail="left", bound = NULL, df = 1, prob = .5,
                       trials = 10, data = NULL, X = NULL, Y = NULL, display_prob = FALSE){
  ## Below are some error checks.
  if(display_prob == TRUE & is.null(bound)){
    if(type != "regression"){
      stop(paste("Error: The display_prob option requires a value for bound as an input."))
    }
  }

  if(type != "normal"
     & type !=  "binomial"
     & type !=  "t-dist"
     & type !=  "chi-squared"
     & type !=  "QQ"
     & type !=  "regression"){
    stop(paste("Error: Your type choice is not an option. Please choose either:
               'normal', 'binomial', 't-dist', or 'chi-squared', 'QQ', or 'regression'"))
  }

  if(tail != "left"
     & tail !=  "right"
     & tail !=  "inner"
     & tail !=  "outer"
     & tail != "two"
     & tail != "left_no_equal"
     & tail != "right_not_equal"){
    stop(paste("Error: Your tail choice is not an option. Please choose either 'left','right','inner','two', 'right_not_equal', or 'left_not_equal'"))
  }
  ## Help options are below
  if(type == "help"){
    stop(paste("Below is a list of graphics this function can build along with their inputs:
               Inputs defined by lists are displaying the options.

               Normal Distribution:
               type = 'normal', bound = c(left,right), tails = list('left','right','inner','two')

               Student's t-distribution
               type = 't-dist', bound = c(left,right), df = 1, tails = list('left','right','inner','two')

               Chi-Squared Distribution
               type = 'Chi-Squared', bound = left, df = 1, tails = list('left','right','inner','two')

               Binomial Distribution
               type = 'binomial', bound = c(left,right), prob = .5, trials = 10
                  tails = list('left','right','inner','two','left_not_equal','right_not_equal','inner_not_equal','two_not_equal')
               "))
  }

  #This piece is to read in and tails and apply value for the polygon below
  if(is.null(bound) & type != "binomial"){
    lower <- -5
    upper <- 5
    fill <- seq(-5,upper,.01)
  }
  if(!is.null(bound) & type != "binomial"){
    if(tail == "left"){
      lower <- -500
      upper <- bound
      fill <- seq(lower,upper,.01)
    }else if(tail == "right"){
      lower <- bound
      upper <- 500
      fill <- seq(lower,upper,.01)
    }else if(tail == "inner"){
      lower <- bound[1]
      upper <- bound[2]
      fill <- seq(lower,upper,.01)
    }else if(tail == "outer" | tail == "two"){
      if(length(bound) == 1){
        bound <- c(-bound,bound)
      }
      lower <- -500
      upper <- bound[1]
      fill <- seq(lower,upper,.01)
      lower_right <- bound[2]
      upper_right <- 500
      fill_right <- seq(lower_right,upper_right,.01)
    }
  }
  ## The below uses logical subsetting to color the graphs. Logical being 0 + 1, or 1 + 1,
  ## Two and One receive a different color layout in the polynomial script for the binomail.
  if(!is.null(bound) & type == "binomial"){
    x <- 0:(trials)
    if(tail == "left"){
      logical <- ((x <= bound) + 1)
    }else if(tail == "right"){
      logical <- ((x >= bound) + 1)
    }else if(tail == "left_not_equal"){
      logical <- ((x < bound) + 1)
    }else if(tail == "right_not_equal"){
      logical <- ((x > bound) + 1)
    }else if(tail == "two" | tail == "outer"){
      logical <- ((x <= bound[1] | x >= bound[2]) + 1)
    }else if(tail == "inner"){
      logical <- ((x >= bound[1] & x <= bound[2]) + 1)
    }else if(tail == "two_not_equal" | tail == "outer_not_equal"){
      logical <- ((x < bound[1] | x > bound[2]) + 1)
    }else if(tail == "inner_not_equal"){
      logical <- ((x > bound[1] & x < bound[2]) + 1)
    }
  }else{logical <- rep(1,length(trials))}

  ## The below section will print a filled in Normal, T-Distribution, or Chi-Squared plot.
  ## I need to add Binomial, Uniform, and Geometric.
  par(bg="linen")
  if(type == "normal"){
    plot(x<-seq(-3.5,3.5,.01),dnorm(x),col="#5a95b3",lwd=2,type="l",main="Normal Plot",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dnorm(fill,0,1),0),border = NA, col = "#5a95b3")
    if(tail == "outer" | tail == "two"){
      polygon(x = c(lower_right,fill_right,upper_right),y = c(0, dnorm(fill_right,0,1),0),border = NA, col = "#5a95b3")
    }
    if(display_prob == TRUE){
      probability <- find_probs(bound = bound, type = "normal", tail = tail)
      text(2.5,.3,paste("Prob: ",round(probability,digits = 3),set = ""))
    }
  }

  if(type == "t-dist"){
    plot(x<-seq(-4,4,.01),dt(x,df),col="#5a95b3",lwd=2,type="l",main="Student's t Plot",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dt(fill,df),0),border = NA, col = "#5a95b3")
    if(tail == "outer" | tail == "two"){
      polygon(x = c(lower_right,fill_right,upper_right),y = c(0, dt(fill_right,df),0),border = NA, col = "#5a95b3")
    }
    if(display_prob == TRUE){
      probability <- find_probs(bound = bound, type = "t-dist", tail = tail, df = df)
      text(2.5,.25,paste("Prob: ",round(probability,digits = 3),set = ""))
    }
  }

  if(type == "chi-squared"){
    ## Refits the bounds to ensure shading occurs below the graph.
    ## This is not designed to work on tails other then "right" or "left"
    if(tail == "left" | is.null(tail)){
      lower <- 0.0000001
      fill <- seq(lower,upper,.01)
    }
    plot(x<-seq(0,4.5 * df,.01),dchisq(x,df),col="#5a95b3",lwd=2,type="l",main="Chi-Squared Distribution",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dchisq(fill,df),0),border = NA, col = "#5a95b3")

    if(display_prob == TRUE){
      probability <- find_probs(bound = bound, type = "chi-squared", tail = tail, df = df)
      text(3 * df,.6 * (max(dchisq(x,df))),paste("Prob: ",round(probability,digits = 3),set = ""))
    }
  }

  if(type == "binomial"){
    color <- c("salmon1","#5a95b3")
    names <- c(0:trials)
    barplot(dbinom(0:trials,trials,prob), ylab = "Probability", xlab = "# of Successes",
            main = paste("Binomial Distribution n = ", trials, "p = ", prob, sep = " "), col = color[logical],
            names.arg=names)

    if(display_prob == TRUE){
      probability <- find_probs(bound = bound, type = "binomial", tail = tail, prob = prob, trials = trials)
        if(prob < .5){
          x_placement <- (3 / 4) * trials
        }else if(prob > .5){
          x_placement <- (1 / 3) * (trials)
        }else if(prob == .5){
          x_placement = (44/45) * trials
        }
      text(x_placement, .8 * max(dbinom(0:trials,trials,prob)),paste("Prob: ",round(probability,digits = 3),set = ""))
    }
  }

  if(type == "QQ"){
    if(is.null(data)){
      stop(paste("Error: Please include a data set to check against the Normal."))
    }

    # Building initial conditions
    n = length(data)
    i = seq(1:n)
    u=(i-.5)/n
    z=sort(qnorm(u))

    # Building the 4 part graph to assess normality.
    par(mfrow=c(2,2),bg="linen")
    plot(z, sort(data), xlab="Perfect Normal", ylab="Data's Quantile's", main="QQ Plot", col="#5a95b3", pch = 16)
    abline(lm(sort(data)~z),col="salmon1",lwd = 2)

    hist(data, main="Actual Histogram of the data", col = "#5a95b3")

    plot(density(data),main="Estimated Histogram of the data", col="salmon1", lwd = 2, xlab = "Propotion")
    plot(x<-seq(-3.5,3.5,.01),dnorm(x),col="#5a95b3",lwd=2, xlab = "Z-Scores", ylab = "Propotion")
  }

  if(type == "regression"){
    if(is.null(data)){
      if(is.null(X) & is.null(Y)){
        stop(paste("Error: Please input both an X and Y data set."))
      }
    }

    if(!is.null(data)){
      data <- as.matrix(data)
      X <- data[,1]
      Y <- data[,2]
    }else if(length(X) == length(Y)){
      X <- X
      Y <- Y
    }else{
      stop(paste("Error: Please input a valid data set into either:
                 X = c() and Y = c()
                 or data = matrix()"))
    }

    plot((Y ~ X), pch = 16, col = "#5a95b3", main = "Regression")
    abline(lm(Y ~ X), lwd = 2, col = "salmon1")

    if(display_prob == TRUE){
      return(get_measures(X = X, Y = Y, regression = TRUE))
    }

  }
}

