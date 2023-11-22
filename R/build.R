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



