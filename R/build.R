##usethis::use_package("ggplot2", type = "Imports")

##This function is used only to allow the user to input graphic options in the list.
##The function will contain the default data settings. It would be nice to have some basic templetes that can
##be used as well. The main and labels will be difficult.

Options <- function(type,xlim,ylim,main,xlab,ylab,sub,frame.plot,number_of_plots,color,pch,cex,bg,lty,lwd){
  ## Create a running color template for use in bar or pie graphs. They can choose the theme.
  palette_ap <- c("wheat","#5a95b3","#b2c8df","#ddb695","#ac754c","#714423")
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
build.dist <- function(type="normal",tail="left",bound = NULL,df,prob,trials,display_prob=FALSE){
  ## Add in errors to print list of options

  #This piece is to read in and tails and apply value for the polygon below
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
    if(tail == "left"){
      logical <- ((x <= bound) + 1)
    }else if(tail == "right"){
      logical <- ((x >= bound) + 1)
    }else if(tail == "left_not_equal"){
        logical <- ((x < bound) + 1)
      }else if(tail == "right_not_equal"){
        logical <- ((x > bound) + 1)
    }
  }

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


export(build)
