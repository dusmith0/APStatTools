#' Title find_conf aka find_confidence intervals
#'
#' @param type Input String: any of
#'        "t_conf.one"
#'        "t_conf.two"
#'        "z_conf.one"
#'        "z_conf.two"
#'        "help"
#'
#' @param point Input numeric vector: This is the point-estimate for the Wald interval.
#'              it can be a single value or a vector of length 2. This function will
#'              assume a difference of means/proportions CI if a length of 2 is given.
#'              It will fallow the calculation of point[1] - point[2] given
#'              point <- c(point[1],point[2])
#' @param sigma Input numeric vector: This is the standard deviation.
#'              it can be a single value or a vector of length 2. This function will
#'              assume a difference of means/proportions CI if a length of 2 is given.
#'              It will fallow the calculation of sigma[1] - sigma[2] given
#'              sigma <- c(sigma[1],sigma[2])
#' @param level Input numeric: It can be any value between 0 and 1. This function
#'              expects the value to be of length one. Imputing more significance levels
#'              will cause the function to override them and return only the last value given.
#' @param n Input numeric: Length of sample size. This will calculate Standard Error for you.
#'
#' @return Data.frame containing only $lower_bound and $upper_bound
#' @export
#'
#' @examples
#'
#'   find_conf(type = "t_conf.one", point = 12, sigma = 3, level = c(.95), n = 20)
#'   find_conf(type = "t_conf.two", point = c(12,10), sigma = c(3,2), level = .95, n = c(20,30))
#'   find_conf(type = "z_conf.one", point = .4, level = .90, n = 20)
#'   find_conf(type = "z_conf.one", point = .4, level = .90, n = 20)
#'
find_conf <- function(type = "help", point, sigma = 0, level, n){
  if(type == "help"){
    stope(paste("Help:Please enter one of the following:
    ('t_conf.one', point, sigma, level, n, df)
    ('t_conf.two',, point, sigma, level, n, df)
    ('z_conf.one',, point, level, n)
    ('z_conf.two', point, level, n)
    ('help')
    "))
  }

  if(type == "z_conf.one" | type == "z_conf.two"){
    if(any(point >= 1)){
      stop(paste("Error: This function requires that the point estimate is a decimal number."))
    }
  }

  if(type == "t_conf.one"){
    df <- n - 1
  }else if(type == "t_conf.two"){
    df <- ((sigma[1] ^ 2 / n[1] + sigma[2] ^ 2 / n[2]) ^ 2) / sum((1 / (n - 1)) * (sigma ^ 2 / n) ^ 2)
  }

  # Building standard error
  if(type == "t_conf.one"){
    se <- sigma / sqrt(n)
  }else if(type == "t_conf.two"){
    se <- sqrt(sigma[1] ^ 2 / n[1] + sigma[2] ^ 2 / n[2])
  }else if(type == "z_conf.one"){
    se <- sqrt(point * (1 - point) / n)
  }else if(type == "z_conf.two"){
    se <- sqrt((point[1] * (1 - point[1]) / n[1]) + (point[2] * (1 - point[2]) / n[2]))
  }

  # Finding critical value
  if(type == "t_conf.one" | type == "t_conf.two"){
    critical <- qt(level + .5 * (1 - level), df)
  }else if(type == "z_conf.one" | type == "z_conf.two"){
    critical <- qnorm(level + .5 * (1 - level),0,1)
  }

  # Calculating the confidence intervals
  interval <- c(0,0)
  if(length(point) == 1){
    interval <- point + c(-1,1) * critical * se
  }else if(length(point) == 2){
    interval <- diff(rev(point)) + c(-1,1) * critical * se
  }else{
    stop(paste("Error: Please input a vector of length one or two for point."))
  }

  return(data.frame(lower_bound = interval[1], upper_bound = interval[2]))

}
