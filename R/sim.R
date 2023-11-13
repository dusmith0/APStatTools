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
Monty_fun <- function(choice = NULL, switch = NULL){
  door <- sample(c(1,0,0),3,replace=FALSE)
  options <- c(1,2,3)
  if(is.null(choice)){
    choice <- readline(prompt = "Choose a door number 1,2,3")
  }

  if((door[choice] == 1){
    options[-choice]
    print(door[])
  }
}


###Sampling Distribution



### 21 and safe Busting
### Note the graphic is not the best tool ever. It does not display dot of large trials e.g. more than 100.
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
  bjack_table[,3] <- data.frame(Perc = (jack_table$Freq/trials))
  return(list(time_to_bust = time_to_bust,bjack_table = bjack_table))
}




