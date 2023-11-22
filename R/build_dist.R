##----------------------------------------------------------------------------##

#' Title build_dist()
#'
#'
#' Description: This function is designed to build a set of distributions and shade in
#' the various regions of interest.
#'
#' Note: This builds only standardized distributions. It assumes the mean=0 and sd=1 in many situations.
#'
#' @param type string being any of "normal", "t-dist", "chi-squared", "binomial"
#' @param tail string being any of "left", "right", "inner", "outer" or "two", "left_not_equal", "right_not_equal"
#'        Notes: "left_not_equal" and "left_not_equal" are intended for the "binomial" only.
#'        Notes: "out" and "two" will produce the same output.
#' @param bound vector of length one or two: c(left,right). This is to define the boundaries of the shaded region of the graph.
#'        Notes: "inner" and "two" require both the left and right input.
#' @param df numeric single value for the degree of freedom
#' @param prob numeric value for the probability of success. Imputing values of greater than 1 may cause errors.
#' @param trials numeric value for the number of trials that will occur for the "binomial"
#' @param display_prob logical If true this will print the probability of the shaded region along with the graphic.
#'
#' @return Graph using the plot() function, along with an optional numeric vector of one.
#' @export
#'
#' @examples
build_dist <- function(type="normal",tail="left",bound = NULL,df = 1,prob = .5,trials = 10,display_prob=FALSE){
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
      lower <- -50
      upper <- bound
      fill <- seq(lower,upper,.01)
    }else if(tail == "right"){
      lower <- bound
      upper <- 50
      fill <- seq(lower,upper,.01)
    }else if(tail == "inner"){
      lower <- bound[1]
      upper <- bound[2]
      fill <- seq(lower,upper,.01)
    }else if(tail == "outer" | tail == "two"){
      if(bound <= 0){
        bound <- -1*bound
      }
      lower <- -50
      upper <- -bound
      fill <- seq(lower,upper,.01)
      lower_right <- bound
      upper_right <- 50
      fill_right <- seq(lower_right,upper_right,.01)
    }
  }

  if(!is.null(bound) & type == "binomial"){
    x <- 1:trials
    if(tail == "left"){
      logical <- ((x <= bound) + 1)
    }else if(tail == "right"){
      logical <- ((x >= bound) + 1)
    }else if(tail == "left_not_equal"){
      logical <- ((x < bound) + 1)
    }else if(tail == "right_not_equal"){
      logical <- ((x > bound) + 1)
    }
  }else{logical <- rep(1,length(trials))}

  ## The below section will print a filled in Normal, T-Distribution, or Chi-Squared plot.
  ## I need to add Binomial, Uniform, and Geometric.
  par(mfrow = c(1,1), bg="wheat")
  if(type == "normal"){
    plot(x<-seq(-3.5,3.5,.01),dnorm(x),col="#5a95b3",lwd=2,type="l",main="Normal Plot",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dnorm(fill,0,1),0),border = NA, col = "#5a95b3")
    if(tail == "outer" | tail == "two"){
      polygon(x = c(lower_right,fill_right,upper_right),y = c(0, dnorm(fill_right,0,1),0),border = NA, col = "#5a95b3")
    }
  }

  if(type == "t-dist"){
    plot(x<-seq(-4,4,.01),dt(x,df),col="#5a95b3",lwd=2,type="l",main="Student's t Plot",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dt(fill,df),0),border = NA, col = "#5a95b3")
    if(tail == "outer" | tail == "two"){
      polygon(x = c(lower_right,fill_right,upper_right),y = c(0, dnorm(fill_right,0,1),0),border = NA, col = "#5a95b3")
    }
  }

  if(type == "chi-squared"){
    ## Refits the bounds to ensure shading occurs below the graph.
    ## This is not designed to work tails other then "right" or "left"
    if(tail == "left" | is.null(tail)){
      lower <- 0.0000001
      fill <- seq(lower,upper,.01)
    }
    plot(x<-seq(0,4.5 * df,.01),dchisq(x,df),col="#5a95b3",lwd=2,type="l",main="Chi-Squared Distribution",
         xlab = "Z-scores",ylab="Probability")
    polygon(x = c(lower,fill,upper),y = c(0, dchisq(fill,df),0),border = NA, col = "#5a95b3")
  }

  if(type == "binomial"){
    color <- c("#b2c8df","#5a95b3")
    names <- c(0:trials)
    barplot(dbinom(0:trials,trials,prob), ylab = "Probability", xlab = "# of Successes",
            main = paste("Binomial Distribution n = ", trials, "p = ", prob, sep = " "), col = color[logical],
            names.arg=names)

  }

}

export(build_dist)
