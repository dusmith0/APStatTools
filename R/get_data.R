#' Title get_data
#'
#' @param data_name Input String. This can either be a data set name in apdata, or your own file path.
#' If you leave this as the default "Empty" you will be prompted to input your data file name.
#' Here is a list of available data sets:
#'  "fastfood2.csv"
#'  "fastfood3.csv"
#'  "indianrestaurant.csv"
#'  "indianrestaurant2.csv"
#'  "Moneyball.txt"
#'  "Moneyball(titled).txt"
#'  "movies.csv"
#'  "potter.csv"
#'  "vacations.csv"
#'  "videogames.csv"
#'  "HurricaneCindy05.xlsx"
#'  "HurricaneKatrina05.xlsx"
#'  "USPovertyLevels.xlsx"
#'  "YoutuberPay.xlsx"
#'
#'  Please type the data sets above exactly as seen. The function should produce an error otherwise.
#'
#' @param ap_data_set Input Logical. If TRUE the function will assume you want to use one of the above data sets.
#' Change the value to FALSE if you wish to work with you own data sets. As of now this will work for
#' either .csv, .txt, .xls, and .xlsx files.
#'
#' @param get_pdf Input Logical. Change to TRUE if you want to data to be saved as a pdf file to your system.
#' @param path Input String. If get_pdf is FALSE please leave this as NULL. If get_pdf is TRUE, you can use this
#' to input the location you want your pdf saved to. You will also be prompted for a path if this is FALSE and get_pdf is TRUE.
#'
#' @return Output Data.Frame called data.set. You will need to explicitly assign the data to a named location inorder to use the data later.
#' That is data <- get_data("potter.csv")
#' @export
#'
#' @examples
#' # Easy use for prebuilt data.
#' get_data("potter.csv")
#'
#' # To allow for saving of the data.
#' get_data("USPovertyLevels.xlsx",get_pdf = TRUE)
#'
#'
#'
get_data <- function(data_name = "Empty", ap_data_set = TRUE, get_pdf = FALSE, path = NULL){
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
       & data_name != "USPovertyLevels.xlsx" & data_name != "YoutuberPay.xlsx" & data_name != "Empty"){
      stop(paste("Error: Your input does not match one of the provided data.sets. Please type get_data('help')."))
    }
   ## The section will read in the preset data sets.
   if(ap_data_set == TRUE){
    location <- system.file("extdata", package = "APStatTools")
    ## reading or writing in the file name.

    if(data_name == "Empty"){
      data_file <- readline(message("Please type in the data set including the .extention:") )
      n <- nchar(data_file)
    }else{
      data_file <- data_name
      n <- nchar(data_file)
    }


      ## Checking for the extensions
      if(!(grepl(".",data_name,fixed = TRUE))){
        stop(paste("Error: Please ensure to include your .extention for your data name. Such as .csv or .tst"))
      }

      ## Reading in different types of data sets
      if(substr(data_file,(n-2),(n)) == "txt"){
        data.set <- try(read.table(paste(location,data_file,sep="/")))
      }

      if(substr(data_file,(n-2),(n)) == "csv"){
        data.set <- try(read.csv(paste(location,data_file,sep="/"),header = T))
      }

      if(substr(data_file,(n-3),(n)) == "xlsx" | substr(data_file,(n-2),(n)) == "xls"){
        data.set <- try(readxl::read_excel(paste(location,data_file,sep="/")))
        data.set <- as.data.frame(data.set)
      }
    }
  ## A wrapper for reading in user created data files.
  if(ap_data_set == FALSE){
    if(data_name == "Empty"){
      data_file <- readline(message("Please type or paste your entire file path here:\n"))
      n <- nchar(data_file)
    }else{
      data_file <- data_name
      n <- nchar(data_file)
    }

    if(substr(data_file,(n-2),(n)) == "txt"){
      data.set <- try(read.table(data_file))
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




