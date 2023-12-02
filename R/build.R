##usethis::use_package("ggplot2", type = "Imports")
library(ggplot2)
library("ggfortify")
##This function is used only to allow the user to input graphic options in the list.
##The function will contain the default data settings. It would be nice to have some basic templetes that can
##be used as well. The main and labels will be difficult.

Options <- function(type,xlim,ylim,main,xlab,ylab,sub,frame.plot,number_of_plots,color,pch,cex,bg,lty,lwd){
  ## Create a running color template for use in bar or pie graphs. They can choose the theme.
  palette_ap <- c("wheat","#5a95b3","#b2c8df","#ddb695","#ac754c","#714423")
}

types <- list(C("Hist","Bar","Pie","Dot","Box","Multi-Box","QQ","Residual","Regression","Multi-regression","Distrubution",
                "segmented","stacked","side-by-side","Mosaic","Back-Back","Pyramid"))

##-----------------------------------------------------------------------------##
# Note: This function is not designed to clean your data. The data should be first assigned using
# Create() then place in the name for data1, data2...
build <- function(input = FALSE, data, X, Y = NULL, type = "Hist", analysis=FALSE){
  ## Build an 'options' selection for usage in default plots
    ##if dedicated term exists in Options() functions set values, else set default values.
    ##likely easiest to place those options in par() it may change with GGplot2,
    ##alternatively try grid::grid.arrange() or grid::grid.layout

##----------------------------------------------------------------------------##
  ##Options and error checks are built below here.


  if(input == TRUE){
    data <- data.frame()
    data <- edit(data)
    data <- as.data.frame(data)
  }

  theme <- ggplot2::theme(panel.background = element_rect(fill = '#E5C3A6'),
        panel.grid.major = element_line(color = '#714423', linetype = 'dotted'),
        panel.grid.minor = element_line(color = '#714423'))

##----------------------------------------------------------------------------##
  ##Graphics are built below here.
  ## Histogram
  ##// Inputs   type, bins,
  if(type == "Hist"){
    if(is.null(bins)){
      bins <- ceiling((nrow(data) / 10))
    }else{
      bins <- bins
    }
    ggplot(data, aes_string(x = X)) +
      geom_histogram(bins = bins, fill = "#4B527E") +
      theme
  }

  ## Bar
  ##//data
  if(type == "Bar"){
    ggplot(data, aes_string(X, fill = X)) +
      geom_bar() +
      theme
  }

  ## Dot Plot,
  if(type == "Dot"){
    ggplot(data, aes_string(X, fill = X), size = 4) +
      geom_dotplot() +
      theme
  }

  ## Box-Plot

#----## This one needs a lot of work done to it. ##--------------------------
  if(type == "Box"){
    ggplot(data, aes_string(X), size = 4) +
      geom_boxplot() +
      theme
  }

  ## Pie
  if(type == "Pie"){
    ggplot(data, aes(X)) +
      geom_bar() +
      coord_polar(theta = Y)
  }

  ## Mosaic

  ## Stacked

  ## Segmented

  ## Back to Back


  ##-------------------------------------------------------------------------##
  ## regression
  if(type == "Regression"){
    ggplot(data, aes(x = X, y = Y)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme
  }

  ## regression diagnosis.
  if(type == "Regression_diag"){
    ggplot2::autoplot(lm(Y ~ X, data = data), label.size = 3) +
      theme
  }
  ## Allow for logarithmic transformation
  if(type == "Regression_log"){
    X <- log(X)
    ggplot(data, aes(x = X, y = Y)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      theme
  }


  ##-------------------------------------------------------------------------##
  ## Density
  if(type == "Density"){
    ggplot(as.numeric(data), aes_string(X)) +
    geom_area()
  }

  ## Tests

  ## QQ Plots vs specified distributions, option to include Wilcoxon test value.

  ##



  ##Include a section to print a generic list of analysis. 5 number summary in a pretty mannor.
  ##If possible a dedicated AP analysis function.
  if(analyis==TRUE){

  }

  plot(X)
}



