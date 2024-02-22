
#' Title Birthday_fun
#'
#' @param Iter Input numeric: This is the number of iterations the function will run through.
#'             Note: The function can be quite slow if Iter >= 1000. However, 200 is more
#'             then enough to show convergence.
#' @param size Input numeric vector: This is a vector containing the different
#'             people group sizes you would like. Default is c(10,20,30,40)
#'             Note: Values below 50 work best.
#' @param plot Input logical: Setting this to FALSE will suppress the graphing.
#'
#' @return Output data.frame of each size value and their running probabilities.
#'
#'
#' @export
#'
#' @examples
#'
#' Birthday_fun()
#'
#' Birthday_fun(Iter = 100, size = c(1,5,10,15,20))
#'
Birthday_fun <- function(Iter = 50, size = NULL, plot=TRUE){
  #Figure out how to make the size allow many inputs with c.
  if(is.null(size)){
    size <- c(10,20,30,40)
  }

  prob <- as.data.frame(matrix(ncol=length(size),nrow=(Iter)))
  test <- prob
  names(prob) <- as.character(size)

  par(mfrow=c(2,2),bg="linen")
  for(j in seq_along(size)){
    for(i in 1:Iter){
      birthdays <- sample(seq(1:365),size[j],replace = TRUE)
      ifelse(length(unique(birthdays)) == length(birthdays),test[i,j]<-0,test[i,j]<-1)
      prob[i,j] <- sum(test[1:i,j])/i

    }
    if(plot == TRUE){
      plot(prob[,j],
           main=paste("Size",size[j]),
           xlab="interations",
           ylab="prob",
           xlim=c(1,Iter),
           ylim=seq(0,1),
           col=palette()[3],
           pch=16)
      abline(h = mean(prob[,j]),
             col = "salmon1",
             lwd=2)
    }
  }
 return(data.frame(Size = prob))
}


#' Title card_count
#'       This function will calculate card values and is needed for other simulation
#'       functions in the APStatTools package.
#'
#' @param draw Input String: A list of drawn card values
#'        being "2","3","4","5","6","7","8","9","A","J","K","Q"
#' @param draw_values Input numeric: This is for a previously defined total card values.
#'        You will usually leave this as 0
#'
#' @return Output list of c((draw, draw_values, value))
#'         draw = your cards
#'         draw_values = each card's worth
#'         value = the total worth.
#' @export
#'
#' @examples
#'
#' card_count(draw = c("2","3","4"))
card_count <- function(draw, draw_values = 0){
  # counting total points
  for(k in 1:length(draw)){
    if(any(draw[k] %in% c("2","3","4","5","6","7","8","9"))){
      draw_values[k] <- as.numeric(draw[k])
    }else if(draw[k] != "A"){
      draw_values[k] <- 10
    }else{
      draw_values[k] <- 1
    }
  }
  value <- sum(draw_values)
  return(list(draw = draw, draw_values = draw_values, value = value))
}


#' Title blakcjack_bust_auto
#'
#'    This will simulate drawing hand of BlackJack and count the number of draws
#'    needed to "Bust:.
#' @param trials Input numeric length 1. It is the number of trials of the game.
#'        It is not recommended to make the trials much more than 50
#'        as the graph will not display that many dots well.
#'
#' @return list of c(time_to_bust, data)
#'         time_to_bust = List of draws need to "bust"
#'         data = list of "time_to_bust", Frequency of the number of cards
#'         at "bust", and probabilities.
#' @export
#'
#' @examples
#'
#' blackjack_bust_auto()
#' blackjack_bust_auto(trials = 25)
#'
#'
blackjack_bust_auto <- function(trials = 10){
  ##setting values
  cards <- c(rep(seq(2,9,1),4),rep(c("A","J","K","Q"),4)) ### list of all of the card values.
  time_to_bust <- c(0)
  hands <- matrix(0,nrow=)

  for(i in 1:trials){
    draw <- sample(cards,2)
    draws <- 2
    value <- 0
    draw_values <- c(0)
    for(j in 1:10){
      # counting total points
      for(k in 1:length(draw)){
        if(any(draw[k] %in% c("2","3","4","5","6","7","8","9"))){
          draw_values[k] <- as.numeric(draw[k])
        }else if(draw[k] != "A"){
          draw_values[k] <- 10
        }else{
          draw_values[k] <- 1
        }
      }

      value <- sum(draw_values)
      if(value > 21){
        break
      }else{
        draw[j + 2] <- sample(cards,1)
        draws <- draws + 1
      }
    } #End loop for one game
    time_to_bust[i] <- draws
    #end loop for many games
  }
  ## Building a graphic
  par(bg = "linen")
  stripchart(time_to_bust,
             method = "stack",
             pch = 19,
             at = .08,
             cex = 1,
             col = "#5a95b3",
             xlab = "Draws to bust\nIncluding initial draw",
             ylab = "Frequency",
             main="Black Jack draws to Bust")
  ## Building a subspace for the probabilities to fit.
  bjack_table <- data.frame(table(time_to_bust))
  bjack_table[,3] <- data.frame(Perc = (bjack_table$Freq/trials))
  return(list(time_to_bust = time_to_bust, data = bjack_table))
}


#' Title blackjack_bust
#'
#'  This is an Input Driven Black-Jack Bust Simulation version of
#'  blackjack_bust_auto()
#'  Note: at the moment this function does not produce graphics automatically.
#'
#' @return list of c(time_to_bust, data)
#'         time_to_bust = List of draws need to "bust"
#'         data = list of "time_to_bust", Frequency of the number of cards
#'         at "bust", and probabilities.
#' @export
#'
#' @examples
#'
#' blackjack_bust()
#'
blackjack_bust <- function(){
  ##setting values
  cards <- c(rep(seq(2,9,1),4),rep(c("A","J","K","Q"),4)) ### list of all of the card values.
  time_to_bust <- c(0)
  hands <- matrix(0,nrow=)
  input <- "Run"
  draw_values <- c(0)
  value <- 0

  ## Act of drawing cards
  draw <- sample(cards,1)
  draws <- 1

  while(value != "Stop"){
    draw <- append(draw,sample(cards,1),after = length(draw))
    draws <- draws + 1

    ## reading and printing card values.
    count <- card_count(draw, draw_values)
    draw_values <- count$draw_values
    print(count)

    if(count$value > 21){
      print("BUSTED!!!!!")
      break()
    }

    ## Checking user input for Hit or to stop playing.
    input <- readline(prompt = "Please type 'Hit' to draw another card or 'Stop' to stop\n >>>")
  }
}


#' Title qq_dem
#'
#'    This is to demonstrate the effectiveness of the QQ plots against various distributions.
#' @return Plot:
#'         This plot will build a lot of QQ plots vs normal plots, and histograms of the simulated data.
#'         In Rstudio you can click through the arrows to look through various examples. In R, click on the graph to see the next on.
#' @export
#'
#' @examples
#'
#' qq_dem()
#'
#'
qq_dem <- function(){
  plot_QQ <- function(data){
    build_dist(type = "QQ", data = data)
  }

  ### Here are some random data points to apply with the function.
  ## I would not suggest changing the sample size to more than 100, as the graph
  ## becomes very difficult to read.
  data <- rnorm(10)
  plot_QQ(data)
  data <- rnorm(100)
  plot_QQ(data)
  data <- rnorm(1000)
  plot_QQ(data)
  data <- rbeta(100,1,.1)
  plot_QQ(data)
  data <- rbinom(100,100,.2)
  plot_QQ(data)
  data <- rbinom(100,100,.8)
  plot_QQ(data)
  data <- rgamma(100,1,1)
  plot_QQ(data)
  data <- runif(100,-3,3)
  plot_QQ(data)

}

#' Title confidence_interval
#'
#'    This is designed to build a confidence interval plot.
#'
#' @param mean Input numeric: The mean value of your prior.
#' @param sigma Input numeric: The standard deviation of your prior.
#' @param n Input numeric: The sample size you would like to take.
#' @param level Input numeric: The confidence level between 0 and 1.
#' @param trials Input numeric. Number of times you would like to run the graph.
#'
#' @return Plot
#' @export
#'
#' @examples
#'
#' confidence_interval(mean = 0, sigma = 1, n = 20, level = .95, trials = 20)
#' confidence_interval(mean = 10, sigma = 3, n = 50, level = .90, trials = 30)
#'
confidence_interval <- function(mean = 0, sigma = 1, n = 20, level = .95, trials = 25){
  intervals <- matrix(0,nrow = trials, ncol = 2)
  point <- c(0)
  standard_error <- c(0)
  count <- 0
  for(i in 1:trials){
    pulls <- rnorm(n,mean,sigma)
    point[i] <- mean(pulls)
    standard_error[i] <- sd(pulls)
    found <- find_conf(type = "t_conf.one",
                       point = point[i],
                       sigma = standard_error[i],
                       level = level,
                       n = n)
    intervals[i,1] <- found$lower_bound
    intervals[i,2] <- found$upper_bound
  }

  par(mfrow = c(1,3), bg = "linen")
  plot(x<-seq((mean - 3.5 * sigma),(mean + 3.5 * sigma),.01),
       dnorm(x, mean, sigma),
       col="#5a95b3",
       lwd=2,
       type="l",
       main="Normal Plot",
       xlab = "Z-scores",
       ylab="Proportion")
  plot(density(point),
       xlim = c((mean -2 * sigma),(mean + 2 * sigma)),
       col = "salmon1",
       lwd = 2,
       main = "Sampling",
       xlab = "Z-scores",
       ylab = "Proportion")
  abline(v = mean(point),
         col = "salmon1",
         lty=2,
         lwd = 1)
  abline(v = mean,
         col = "#5a95b3",
         lty=2,
         lwd = 1)

  plot(x = 1,
       xlab = "Intervals",
       ylab = "Count",
       xlim = c((mean -1.5 * sigma),(mean + 1.5 * sigma)),
       ylim = c(0, trials),
       main = "Confidence Intervals",
       type = "n")
  abline(v = mean,
         col = "salmon1",
         lwd = 2,
         lty=2)

  for(i in 1:length(point)){
    segments(x0 = intervals[i,1],
             x1 = intervals[i,2],
             y0 = i,
             col = "#5a95b3",
             lwd = 2)
    points(mean(intervals[i,]),
           i,
           col = "salmon1",
           pch = 16)
    count[i] <- (intervals[i,1] <= mean & intervals[i,2] >= mean)
  }

  mtext(paste("% Hit",sum((count))/trials))

}



#' Title tanks simulation
#'
#' @param max numeric: Should be the Maximum number of tanks in the simulation
#' @param size numeric: This is the sample size being drawn.
#' @param trials numeric: This is the number of trials, or samples being taken
#' @param method string: Any of double mean", "3 sigma", "1.5 IQR Rule"
#' @param show_truth logical: Set to TRUE to show the maximum number of tanks
#' @param show_samples logical: Set to TRUE to show the samples taken
#'
#' @return list containing samples, estimates, estimate mean, estimate sampling standard deviation
#'         a simple confidence interval set at .95 confidence, and the Maximum number of tanks.
#' @export
#'
#' @examples
#' tanks()
#'
#' tanks(max = 500, size = 10, trials = 1000, method = "3 sigma", show_truth = FALSE, show_samples = FALSE)
#'
#'
tanks <- function(max = NULL, size = 5, trials = 50, method = "double mean",
                  show_truth = FALSE, show_samples = FALSE){
  ## Generating Number of tanks
  if(is.null(max)){
    max <- 246 #The first value found in Google :D
  }

  ## Building empty variables
  samples <- matrix(0, ncol = trials, nrow = size)
  estimates <- rep(0,trials)
  Q1 <- 0
  Q3 <- 0
  IQR <- 0

  ## Generating Samples
  for(i in 1:trials){
    samples[,i] <- sample(1:max,5,replace = FALSE)
  }

  ## Method Choice
  if(method == "double mean"){
    estimates <- 2 * rowMeans(samples)
  }else if(method == "2 sigma"){
    estimates <- rowMeans(samples) + 2 * apply(samples,2,function(z) sd(z))
  }else if(method == "1.5 IQR Rule"){
    for(i in 1:ncol(samples)){
      samples[,i] <- sort(samples[,i])
      Q1[i] <- mean(c(samples[floor((size + 1) / 4),i], samples[ceiling((size + 1) / 4),i]))
      Q3[i] <- mean(c(samples[floor(3 * (size + 1) / 4),i], samples[ceiling(3 * (size + 1) / 4),i]))
      IQR[i] <-  Q3[i] - Q1[i]
      estimates[i] <- Q3[i] + 1.5 * IQR[i]
    }
  }

  #Building the Confidence Interval at .95
  est_mean <- mean(estimates)
  est_se <- sd(estimates) / sqrt(size)
  conf <- est_mean + c(-1,1) * pnorm(.975,0,1) * est_se

  ## To allow for suppressing various outputs.
  if(show_truth == FALSE & show_samples == TRUE){
      return(list(samples = samples, estimates = estimates,
              est_mean = est_mean, est_se = est_se, conf = conf))
  }else if(show_truth == FALSE & show_samples == FALSE){
      return(list(estimates = estimates,
                est_mean = est_mean, est_se = est_se, conf = conf))
  }else if(show_truth == TRUE & show_samples == TRUE){
      return(list(samples = samples, estimates = estimates,
                est_mean = est_mean, est_se = est_se, conf = conf, max = max))
  }else if(show_truth == TRUE & show_samples == FALSE){
      return(list(estimates = estimates,
                est_mean = est_mean, est_se = est_se, conf = conf, max = max))
  }
}







