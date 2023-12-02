build_qq <- function(data,vs_dist = "normal", alpha = 1, beta = 1){
  list <- c("normal","gamma","uniform","poisson","binomial","geometric","chi-squared")

  ## Setting up needed values.
  n <- length(data)
  i <-  seq(1:n)
  u <- (i-.5)/n

  ## Defining the Quantiles to test against
  if(vs_dist == "normal"){
    Z <- sort(qnorm(u))
    #vs <- dnorm(1000)
  }else if(vs_dist == "gamma"){
    Z <- sort(qgamma(u,alpha,beta))
    #vs <- dgamma(1000,alpha,beta)
  }else if(vs_dist == "poisson"){
    Z <- sort(qpois(u,alpha))
    #vs <- dpois(1000,alpha)
  }else if(vs_dist == "uniform"){
    Z <- sort(qunif(u,alpha,beta))
    #vs <- dunif(1000,alpha,beta)
  }else if(vs_dist == "uniform"){
    Z <- sort(qgeom(u,alpha))
    #vs <- dgeom(1000,alpha)
  }else if(vs_dist == "chi-squared"){
    Z <- sort(qchisq(u,alpha))
    #vs <- qchisq(1000,alpha)
  }

  par(mfrow=c(1,2),bg="wheat")
  plot(Z, sort(data), xlab="Perfect Normal", ylab="Data's Quantiles", main="QQ Plot", col="#5a95b3", pch = 16)
  abline(lm(sort(data)~Z),col="#714423",lwd = 2)

  #plot(density(vs),main="Estimated Density", col="#714423", lwd = 2)

  hist(data, main="Data Histogram\n Estimated Density", prob = TRUE)
  lines(density(data),main="Estimated Density", col="#714423", lwd = 2)
}
