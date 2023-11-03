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

Birthday_fun <- function(Iter = 50,size = NULL,plot=TRUE){
  #Figure out how to make the size allow many inputs with c.
  if(is.null(size)){
    size <- c(10,20,30,40,50,75,100)
  }

  prob <- as.data.frame(matrix(ncol=length(size),nrow=(Iter)))
  test <- prob
  names(prob) <- as.character(size)

  par(mfrow=c(3,3))
  for(j in seq_along(size)){
    for(i in 1:Iter){
      population <- sample(seq(1:365),size[j],replace = TRUE)
      ifelse(length(unique(population)) == length(population),test[i,j]<-0,test[i,j]<-1)
      prob[i,j] <- sum(test[1:i,j])/i

    }
    if(plot == TRUE){
      plot(prob[,j])
    }
  }

}


