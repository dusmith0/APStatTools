##usethis::use_package("ggplot2", type = "Imports")

##This function is used only to allow the user to input graphic options in the list.
##The function will contain the default data settings. It would be nice to have some basic templetes that can
##be used as well. The main and labels will be difficult.

Options <- function(type,xlim,ylim,main,xlab,ylab,sub,frame.plot,number_of_plots,color,pch,cex,bg,lty,lwd){
  ## Create a running color template for use in bar or pie graphs. They can choose the theme.
}

##-----------------------------------------------------------------------------##
# Note: This function is not designed to clean your data. The data should be first assigned using
# Create() then place in the name for data1, data2...
build <- function(data = list(data),type = list(type),analysis=FALSE){ ##Explore continent methods for imputing multiple data types.
  ## Build an 'options' selection for usage in default plots
    ##if dedicated term exists in Options() functions set values, else set default values.
    ##likely easiest to place those options in par() it may change with GGplot2,
    ##alternatively try grid::grid.arrange() or grid::grid.layout

  ##Split into smaller function per type.
  types <- list(C("Hist","Bar","Pie","Dot","Box","Multi-Box","QQ","Residual","LSRL","Multi-regression","Distrubution",
                  "segmented","stacked","side-by-side","Mosaic","Back-Back","Pyramid"))


  ##Build sub-functions to handle the individual graphic types.
  if(type == "Hist"){

  }
  ##Include an iteration to handle multiple data output.




  ##Include a section to print a generic list of analysis. 5 number summary in a pretty mannor.
  ##If possible a dedicated AP analysis function.
  if(analyis==TRUE){

  }

  plot(X)
}



##----------------------------------------------------------------------------##
## Note: This function uses standardized inputs.
build.dist <- function(type,tail,bound,df,prob=TRUE){

  #This piece is to read in and tails and apply value for the polygon below
  if(tail == "left"){
    lower <- -5
    upper <- bound
    fill <- seq(lower,upper,.01)
  }else if(tail == "right"){
    lower <- bound
    upper <- 5
    fill <- seq(lower,upper.01)
  }else if(tail == "inner")
    lower <- bound[1]
    upper <- bound[2]
    fill <- seq(lower,upper,.01)
  }



  par(bg="wheat")
  if(type == "normal"){
    plot(x<-seq(-3.5,3.5,.01),dnorm(x),col="blue",lwd=2,type="l",main="Normal Plot",
    xlab = "Z-scores",ylab="Probability")
  }

  if(type == "t-dist"){
    plot(x<-seq(-4,4,.01),dt(x,df),col="blue",lwd=2,type="l",main="Student's t Plot",
         xlab = "Z-scores",ylab="Probability")
  }

  if(type == "chi-squared"){
    plot(x<-seq(0,3 * df,.01),dchisq(x,df),col="blue",lwd=2,type="l",main="Student's t Plot",
         xlab = "Z-scores",ylab="Probability")
  }

  polygon(x = c(lower,fill,upper),y = c(0, dnorm(fill,0,1),0),border = NA, col = "blue")

}


export(build)
