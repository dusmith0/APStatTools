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

###The funciton below is designed to simulate the Birthday Problem.
###Note the input for size assumes a vector of somelenghts. Values below 50 are best.
###The function also assumes a lenght of 4 for graphical output.
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




