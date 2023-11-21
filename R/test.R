#This needs to be a large wrapper function.

test <- function(null,x_bar,p_hat,sd,n,df,test,tail="left",data=NULL,graph=TRUE){
  ## Preform some error checks for correct input.


  ##Check to see if the information was input as data,
  ##if so find the mean and sd based on number of columns in the data.
  if(!is.null(data)){

  }

  #maybe define the norm calculations here?

  ##Place a call function here

  ##-------------------------------------------------------------------------##
  #Functions for tests go here-----------------------------------------------##

  return(list(test = "Two Sample z-test", p_value = p_value, test_statistic = test_statistic, x_bar = x_bar, se = se, n = n, df = df))
}


tests <- list("t-test","t-test.paired","z-test.one","z-test.two","z-test.pooled","chi-squared","ANOVA",
              "chi-squared.GOF","chi-squared.homogeneity","chi-squared.independence")

tail <- list("left","right","two")
