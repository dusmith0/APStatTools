#' Title
#'
#' @param data_name
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#' Note: This function will allow you to enter you data set name as
get_data <- function(data_name = NULL,ap_data_set = TRUE){
  if(data_name = "help" | "list"){
    list <- c("fastfood2.csv","fastfood3.csv","indianrestaurant.csv","indianrestaurant2.csv","Moneyball.txt",
              "Moneyball(titled).txt","movies.csv","potter.csv","vacations.csv","videogames.csv")
    stop(cat("Below is a list the avalible data sets. You can also type data() to find many R defined data sets:",list,sep = "\n"))
  }
  if(ap_data_set == TRUE & is.null(data_name)){
    data_file <- readline(message("Please type in the data set including the .extention:") )
    n <- nchar(data_file)
  }else{
    data_file <- data_name
    n <- nchar(data_file)
  }

  if(substr(data_file,(n-2),(n)) == "txt"){#input string check for last three digits of the script. i.e. txt
  data.set <- try(read.table(paste("apdata",data_file,sep="/"),header = T))
  }

  if(substr(data_file,(n-2),(n)) == "csv"){#input string check for last three digits of the script. i.e. csv
  data.set <- try(read.csv(paste("apdata",data_file,sep="/"),header = T))
  }
  ## TO DO create another else for code that inputs EXCELL files.

  ##Include a set of code that allows input from BASE data sets, as well as other packages/
  ##This might be over my head data(package = .packages(all.available = TRUE))

  ##TO DO error code that can in desired print a list of data set names to type in.

  ##TO DO create a function that allows a user to select specific pieces of data from each file.
  ## such as 100 data pieces, or 10, or specific fields of data. Also allow print of data to be viewed.

  ##Create a function that can print a pdf of the data set if deired.
  return(data.set)
}




