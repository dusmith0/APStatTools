#This needs to be a large wrapper function.

test <- function(null,x_bar,p_hat,sd,n,df,test,tail="left",data=NULL,graph=TRUE){
  ## Preform some error checks for correct input.


  ##Check to see if the information was input as data,
  ##if so find the mean and sd based on number of columns in the data.
  if(!is.null(data)){

  }

  #maybe define the norm calculations here?

  ## z_test family of functions
  z.test.one <- function(x_bar,sd,test_value,tail="left"){

    test_statistic <- (test_value - sample_mean)/se

    if(tail == "left"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE)
    }else if(tail == "right"){
      p_value <- pnorm(test_statistic,0,1,lower.tail = FALSE)
    }else{p_value <- pnorm(test_statistic,0,1,lower.tail = TRUE) * 2}

    ##Change the below section to a function in Build for either normal, t, or X-squared.
    plot(x<-seq(-3.5,3.5,.01),dnorm(x),col="Blue",lwd=2,type="l",main="Normal Plot",
         xlab = "Z-scores",ylab="Probability")
    polygon(c(seq(2.5,3.5,.01)),density=NULL)

    return(data.frame(p_value = p_value,test_statistic = test_statistic))

  }

  z.test.two <- function(){

  }

  z.test.pooled <- function(){

  }

  ## t-test family
  t.test.one <- function(){

  }

  t.test.two <- function(){

  }

  ## Chi-squared family


  return(list(p_value = p_value, test_statistic = test_statistic, x_bar = x_bar, se = se, n = n, df = df))
}


tests <- list("t-test","t-test.paired","z-test.one","z-test.two","z-test.pooled","chi-squared","ANOVA",
              "chi-squared.GOF","chi-squared.homogeneity","chi-squared.independence")

tail <- list("left","right","two")
