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
}


data_list <- list("Moneyball","Movies")
