#' Title
#'
#' @param data_name
#'
#' @return
#' @export
#'
#' @examples
get_data <- function(data_name,ap_data_set = TRUE){
  if(ap_data_set == TRUE){
    data_file <- readline()
  }

  if(){#input string check for last three digits of the script. i.e. txt
  data.set <- try(read.table(paste("apdata",data_file,sep="/"),header = F))
  }
  else(){#input string check for last three digits of the script. i.e. csv
  data.set <- try(read.csv(paste("apdata",data_file,sep="/"),header = F))
  }
  ## TO DO create another else for code that inputs EXCELL files.

  ##Include a set of code that allows input from BASE data sets, as well as other packages/
  ##This might be over my head data(package = .packages(all.available = TRUE))

  ##TO DO error code that can in desired print a list of data set names to type in.

  ##TO DO create a fucntion that allows a user to select specific peices of data from each file.
  ## such as 100 data peices, or 10, or specific feilds of data. Also allow print of data to be viewed.

  ##Create a function that can print a pdf of the data set if deired.
}


data_list <- list("Moneyball","Movies")
