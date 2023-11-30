##usethis::use_package("ggplot2", type = "Imports")
library(ggplot2)
##This function is used only to allow the user to input graphic options in the list.
##The function will contain the default data settings. It would be nice to have some basic templetes that can
##be used as well. The main and labels will be difficult.

Options <- function(type,xlim,ylim,main,xlab,ylab,sub,frame.plot,number_of_plots,color,pch,cex,bg,lty,lwd){
  ## Create a running color template for use in bar or pie graphs. They can choose the theme.
  palette_ap <- c("wheat","#5a95b3","#b2c8df","#ddb695","#ac754c","#714423")
}

types <- list(C("Hist","Bar","Pie","Dot","Box","Multi-Box","QQ","Residual","LSRL","Multi-regression","Distrubution",
                "segmented","stacked","side-by-side","Mosaic","Back-Back","Pyramid"))

##-----------------------------------------------------------------------------##
# Note: This function is not designed to clean your data. The data should be first assigned using
# Create() then place in the name for data1, data2...
build <- function(data, x_values, y_values = NULL, type = "Hist", analysis=FALSE){ ##Explore continent methods for imputing multiple data types.
  ## Build an 'options' selection for usage in default plots
    ##if dedicated term exists in Options() functions set values, else set default values.
    ##likely easiest to place those options in par() it may change with GGplot2,
    ##alternatively try grid::grid.arrange() or grid::grid.layout

  ##Split into smaller function per type.

  ##Build sub-functions to handle the individual graphic types.
  ##-------------------------------------------------------------------------##
  ## Histogram
  ##// Inputs   type, bins,
  if(type == "Hist"){
    if(is.null(bins)){
      bins <- ceiling((nrow(data) / 10))
    }else{
      bins <- bins
    }

    ggplot(data, aes_string(x = x_value)) +
      geom_histogram(bins = bins, fill = "#4B527E") +
      theme(panel.background = element_rect(fill = '#E5C3A6'),
                panel.grid.major = element_line(color = '#714423', linetype = 'dotted'),
                panel.grid.minor = element_line(color = '#714423'))
  }

  ## Bar
  ##//data
  if(type == "Bar"){
    ggplot(data, aes_string(x_value, fill = x_value, )) +
      geom_bar() +
      theme(panel.background = element_rect(fill = 'wheat'),
            panel.grid.major = element_line(color = '#714423', linetype = 'dotted'),
            panel.grid.minor = element_line(color = '#714423'))
  }

  ## Dot Plot,
  if(type == "Dot"){
    ggplot(data, aes_string(x_value, fill = x_value), size = 4) +
      geom_dotplot() +
      theme(panel.background = element_rect(fill = 'wheat'),
            panel.grid.major = element_line(color = '#714423', linetype = 'dotted'),
            panel.grid.minor = element_line(color = '#714423'))
  }


  ## Box-Plot

#----## This one needs a lot of work done to it. ##--------------------------
  if(type == "Box"){
    ggplot(data, aes_string(x_value, y_value), size = 4) +
      geom_boxplot() +
      theme(panel.background = element_rect(fill = 'wheat'),
            panel.grid.major = element_line(color = '#714423', linetype = 'dotted'),
            panel.grid.minor = element_line(color = '#714423'))
  }



  ## Pie
  if(type == "Pie"){
    ggplot(data2, aes(species)) +
      geom_bar() +
      coord_polar(theta = Y)
  }

  ##Include an iteration to handle multiple data output.



  ##Include a section to print a generic list of analysis. 5 number summary in a pretty mannor.
  ##If possible a dedicated AP analysis function.
  if(analyis==TRUE){

  }

  plot(X)
}



