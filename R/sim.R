library(gridExtra)

#' Title
#'
#' @param data
#' @param type
#'
#' @return
#' @export
#'
#' @examples
sim <- function(data,type){

}



sims <- list("Birthday","Monty.Hall.Auto","Monty.Hall","Sampling","Law.of.Large.Numbers")

###The function below is designed to simulate the Birthday Problem.
###Note the input for size assumes a vector of somethings. Values below 50 are best.
###The function also assumes a length of 4 for graphical output.
###The function gets slow at around 2000 Iterations. However, 200 is enought to see convergence.
Birthday_fun <- function(Iter = 50,size = NULL,plot=TRUE){
  #Figure out how to make the size allow many inputs with c.
  if(is.null(size)){
    size <- c(10,20,30,40)
  }

  prob <- as.data.frame(matrix(ncol=length(size),nrow=(Iter)))
  test <- prob
  names(prob) <- as.character(size)

  par(mfrow=c(2,2),bg="wheat")
  for(j in seq_along(size)){
    for(i in 1:Iter){
      population <- sample(seq(1:365),size[j],replace = TRUE)
      ifelse(length(unique(population)) == length(population),test[i,j]<-0,test[i,j]<-1)
      prob[i,j] <- sum(test[1:i,j])/i

    }
    if(plot == TRUE){
      plot(prob[,j],main=paste("Size",size[j]),xlab="interations",ylab="prob",xlim=c(1,Iter),
      ylim=seq(0,1),col=palette()[3],pch=16)
      abline(h = mean(prob[,j]),col = "purple",lwd=2)
    }
  }

}


###Monty-Hall functions
#Monty_fun <- function(choice = NULL, switch = NULL){
#  door <- sample(c(1,0,0),3,replace=FALSE)
#  options <- c(1,2,3)
#  if(is.null(choice)){
#    choice <- readline(prompt = "Choose a door number 1,2,3")
#  }
#
#  if((door[choice] == 1){
#    options[-choice]
#    print(door[])
#  }
#}


###Sampling Distribution



### 21 and safe Busting
### Note the graphic is not the best tool ever. It does not display dot of large trials e.g. more than 100.

### This is a function needed to calculate the below simulations.
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
  par(bg = "wheat")
  stripchart(time_to_bust, method = "stack",pch = 19, at = .08, cex = 1, col = "#714423",
             xlab = "Draws to bust\nIncluding initial draw",ylab = "Frequency", main="Black Jack draws to Bust")
  ## Building a subspace for the probabilities to fit.
  bjack_table <- data.frame(table(time_to_bust))
  bjack_table[,3] <- data.frame(Perc = (bjack_table$Freq/trials))
  return(list(time_to_bust = time_to_bust,bjack_table = bjack_table))
}


## Input Driven Black-Jack Bust Similation
## Note: at the moment this function does not produce graphics automatically.
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
    print(count) #Create a nice data frame to print.

    if(count$value > 21){
      print("BUSTED!!!!!")
      break()
    }

    ## Checking user input for Hit or to stop playing.
    input <- readline(prompt = "Please type 'Hit' to draw another card or 'Stop' to stop\n >>>")
  }
}


## QQ-Demonstration
## This plot will build a lot of QQ plots vs normal plots, and histograms of the simulated data.
## In Rstudio you can click through the arrows to look through various examples. In R, click on the graph to see the next on.
qq_dem <- function(){
  plot_QQ <- function(data){
    n = length(data)
    i = seq(1:n)
    u=(i-.5)/n
    z=sort(qnorm(u))

    par(mfrow=c(1,3),bg="wheat")
    plot(z, sort(data), xlab="Perfect Normal", ylab="Data's Values", main="QQ Plot", col="#5a95b3", pch = 16)
    abline(lm(sort(data)~z),col="#714423",lwd = 2)

    plot(density(data),main="Estimated Histogram of the data", col="#714423", lwd = 2)
    lines(x<-seq(-3.5,3.5,.01),dnorm(x),col="#5a95b3",lwd=2) #Note this will fail if the data is not centered around one. Which it will not be many times.

    #This is a quick fix for the problem above. Not a good fix though.
    hist(data, main="Actual Histogram of the data")
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



