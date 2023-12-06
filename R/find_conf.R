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
