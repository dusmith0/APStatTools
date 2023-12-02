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
library(readxl)

get_data <- function(data_name = NULL, ap_data_set = TRUE, get_pdf = FALSE, path = NULL){
  ## For imputing pre-built data in this packages
  if(ap_data_set == TRUE){

    ## A list of the current data sets loaded into this package
    if(data_name == "help"){
      list <- c("fastfood2.csv","fastfood3.csv","indianrestaurant.csv","indianrestaurant2.csv","Moneyball.txt",
              "Moneyball(titled).txt","movies.csv","potter.csv","vacations.csv","videogames.csv","HurricaneCindy05.xlsx",
              "HurricaneKatrina05.xlsx","USPovertyLevels.xlsx","YoutuberPay.xlsx")
      stop(cat("Below is a list the avalible data sets. You can also type data() to find many R defined data sets:",list,sep = "\n"))
    }
    ## Checking to see if the data sets match
    if(data_name != "fastfood2.csv" & data_name != "fastfood3.csv" & data_name != "indianrestaurant.csv" & data_name != "indianrestaurant2.csv" & data_name != "Moneyball.txt"
       & data_name != "Moneyball(titled).txt" & data_name != "movies.csv" & data_name != "potter.csv" & data_name != "vacations.csv"
       & data_name != "videogames.csv" & data_name != "HurricaneCindy05.xlsx" & data_name != "HurricaneKatrina05.xlsx"
       & data_name != "USPovertyLevels.xlsx" & data_name != "YoutuberPay.xlsx"){
      stop(paste("Error: Your input does not match one of the provided data.sets. Please type get_data('help')."))
    }
    ## Checking for the extentions
    if(!(grepl(".",data_name,fixed = TRUE))){
      stop(paste("Error: Please ensure to include your .extention for your data name. Such as .csv or .tst"))
    }
    ## reading or writing in the file name.
    if(is.null(data_name)){
      data_file <- readline(message("Please type in the data set including the .extention:") )
      n <- nchar(data_file)
    }else{
      data_file <- data_name
      n <- nchar(data_file)
    }

    ## Reading in different types of datasets
    if(substr(data_file,(n-2),(n)) == "txt"){
      data.set <- try(read.table(paste("apdata",data_file,sep="/"),header = T))
    }

    if(substr(data_file,(n-2),(n)) == "csv"){
      data.set <- try(read.csv(paste("apdata",data_file,sep="/"),header = T))
    }

    if(substr(data_file,(n-3),(n)) == "xlsx" | substr(data_file,(n-2),(n)) == "xls"){
      data.set <- try(readxl::read_excel(paste("apdata",data_file,sep="/")))
      data.set <- as.data.frame(data.set)
    }
  }

  ## A wrapper for reading in user created data files.
  if(ap_data_set == FALSE){
    if(is.null(data_name)){
      data_file <- readline(message("Please type or paste your entire file path here:\n"))
      n <- nchar(data_file)
    }else{
      data_file <- data_name
      n <- nchar(data_file)
    }

    if(substr(data_file,(n-2),(n)) == "txt"){
      data.set <- try(read.table(data_file),header = T)
    }

    if(substr(data_file,(n-2),(n)) == "csv"){
      data.set <- try(read.csv(data_file))
    }

    if(substr(data_file,(n-3),(n)) == "xlsx" | substr(data_file,(n-2),(n)) == "xls"){
      data.set <- try(readxl::read_excel(data_file))
      data.set <- as.data.frame(data.set)
    }
  }

  ## To save the pdf if desired.
  if(get_pdf == TRUE)
    if(is.null(path)){
      path <- readline(message("Please input your save location path here:\n"))
    }else{
      path <- path
    }
    pdf(path,paper = "a4")
    print(data.set)
    graphics.off()

  ##Create a function that can print a pdf of the data set if desired.
  return(data.set)
}




