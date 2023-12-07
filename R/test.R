#' Title test
#'
#' @param test Input String: Can be any of one of the following
#'        "t_test.one","t_test.two","t_test.paired","z_test.one","z-test.pooled","chi_squared.gof","chi_squared.ind"
#' @param tail Input String: Being one of "left" or "right".
#'         Note: this function will assume two tailed null hypothesis if anything other
#'         then "left" or "right" is used. Be aware that the function will also assume a "left" tail when preforming
#'         the two tailed calculation. To avoid this, set tail to "right" and multiply by 2.
#' @param null Input Numeric: This is a single value for the Null Hypothesis's expected value.
#' @param x_bar Input Vector: This is the value of the sample means. It assumes the standard error adjustment has not been taken.
#'         Note: This can be a vector of one or two values depending on the test used.
#' @param table_one Input Vector: This is a vector of numeric values intended for the
#'         t_test.paired test only. This is the first vector to be compared. The function will assume table_one - table_two.
#' @param table_two Input Vector: This is a vector of numeric values intended for the
#'         t_test.paired test only. This is the second vector to be compared. The function will assume table_one - table_two.
#' @param p_hat Input Vector: This is the value of the sample proportions.
#'         It assumes the standard error adjustment has not been taken, and that the value is in decimal form.
#'         Note: This can be a vector of one or two values depending on the test used.
#' @param sigma Input Vector: This is the value of the sample standard deviations.
#'         Note: This can be a vector of one or two values depending on the test used.
#' @param n Input Vector: This is the value of the sample sizes.
#'         Note: This can be a vector of one or two values depending on the test used.
#' @param level Input numeric: It can be any value between 0 and 1. This function
#'              expects the value to be of length one. Imputing more significance levels
#'              will cause the function to override them and return only the last value given.
#' @param obs_table Input Numeric Vector: This is the observed vector of categorical values.
#'         Note: If the total is included please set row_totals = TRUE
#' @param expected_table Input Numeric Vector: This is a vector of either expected
#'         values or proportions to be computed in Pearson's Chi-Squared test.
#'         If the values are counts and not proportions, please set expected_as_count = TRUE.
#' @param expected_as_count Input Logical. If expected_table is of proportions let this be FALSE.
#'         If the expected_table is of counts let this be TRUE.
#' @param row_totals Input Logical: If the obs_table includes the total, let this be TRUE.
#'         Note: It is defaulted to be FALSE, and will include the total in the calculation
#'         if a total is actually included. This will lead to errors. Also, you will want to ensure
#'         that both the obs_table and expected_table either both have totals or both do not. The
#'         function will either remove the last element of both vectors, or neither vectors.
#' @param mat_totals Input Logical: If FALSE this function will generate the total columns and rows for the user.
#'         However, it will add both the row and column totals. It is not recommended to use this feature
#'         to add just one of the row's or column's totals, as you will end up with an additional unwanted set of totals.
#' @param graph Input Logical: Set this to FALSE if you want to suppress graphing the density function.
#'
#' @return Output: This will return a data.frame of statistical values and a graphic if desired.
#'         the data frame will contain some of the following values, given the appropriate test.
#'         Test Name, p_value, test_statistic, n, standard_error, df, obs_table, expected_count,
#'         and chi_squared_values test statistic values.
#' @export
#'
#' @examples
#' # help
#' test("help")
#'
#' # One sample test on means
#' test(test = "t_test.one", null = 10, x_bar = 11, sigma = 3, n = 25, tail = "left", graph = TRUE)
#'
#' # Two sample test on means
#' test(test = "t_test.two", null = 0, x_bar = c(12,11), sigma = c(2,3), n = c(25,25), tail = "left", graph = TRUE)
#'
#' # Paired t-test
#' X <- c(10,11,12,13,14,15,16,17,18,19,20)
#' Y <- c(12,11,13,14,14,9,15,13,20,14,18)
#' test(test = 't_test.paired', null = 0, table_one = X, table_two = Y, tail = "left", graph = TRUE)
#'
#' # One sample z-test on proportions
#' test(test = 'z_test.one', null = .4, p_hat = .6, n = 10, tail = "left", graph = TRUE)
#'
#' # Two sample z-test on proportions assuming null = 0
#' test(test = 'z_test.pooled', p_hat = c(.4,.5), n = c(25,24), tail = "left", graph = TRUE)
#'
#' # GOF Chi-squared test.
#' X <- c(10,13,14,20,16)
#' Y <- c(.2,.2,.2,.2,.2)
#' test(test = 'chi_squared.gof', obs_table = X, expected_table = Y, expected_as_count = FALSE, row_totals = FALSE, graph = TRUE)
#'
#' # This vector included a total at the end, and has the expected table as a count instead of frequency.
#' # Note the function will assume that both expected an null contain totals.
#' X <- c(10,13,14,20,16,73)
#' Y <- c(8,12,8,16,13,57)
#' test(test = 'chi_squared.gof', obs_table = X, expected_table = Y, expected_as_count = TRUE, row_totals = TRUE, graph = TRUE)
#'
#' # Chi-squared test for independence
#' X <- matrix(rnorm(20,10,3), nrow = 4)
#' test(test = 'chi_squared.ind', obs_table = X, mat_totals = FALSE, graph = TRUE)
#'
#' # Finding confidence Interval
#' test(test = "t_conf.one", x_bar = 12, sigma = 3, level = .95, n = 20)
#' test(test = "t_conf.two", x_bar = c(12,10), sigma = c(3,2), level = .95, n = c(20,30))
#' test(test = "z_conf.one", p_hat = .4, level = .90, n = 20)
#' test(test = "z_conf.two", p_hat = c(.4,.6), level = .99, n = c(20,30))
#'
test <- function(test = "help", tail="left", null, x_bar, table_one, table_two, p_hat, sigma, n, level,
                 obs_table = NULL, expected_table = NULL, expected_as_count = FALSE, row_totals = FALSE, mat_totals = FALSE,
                 graph=TRUE){

  ## This is to allow the user to see what is needed for each test type.
  if(test == "help"){
    stop(paste("Below is a list of the needed inputs for each test type:

               ('t_test.one', null, x_bar, sigma, n, tail, graph)
               ('t_test.two', null, x_bar = c(1,2), sigma, n tail, graph)
               ('t_test.paired', null, table_one, table_two, tail, graph)
               ('z_test.one', null, p_hat, n, tail, graph)
               ('z_test.pooled', p_hat, n, tail, graph)
               ('chi_squared.gof', obs_table, expected_table, expected_as_count, row_totals, graph)
               ('chi_squared.int', obs_table, mat_totals, graph)
               ('t_conf.one', x_bar, sigma, level, n)
               ('t_conf.two',, x_bar = c(1,2), sigma = c(1,2), level, n = c(1,2))
               ('z_conf.one',, p_hat, level, n)
               ('z_conf.two', p_hat = c(1,2), level, n = c(1,2))
               ('help')
               "))
  }
  if(test == "t_test.one"){
    stats <- t_test.one(null = null, x_bar = x_bar, sigma = sigma, n = n, tail = tail, graph = graph)
  }
  if(test == "t_test.two"){
    stats <- t_test.two(null = null, x_bar = x_bar, sigma = sigma, n = n, tail = tail, graph = graph)
  }
  if(test == "t_test.paired"){
    stats <- t_test.paired(null = null, table_one = table_one ,table_two = table_two , tail = tail , graph = graph)
  }
  if(test == "z_test.one"){
    stats <- z_test.one(null = null, p_hat = p_hat, n = n, tail = tail, graph = graph)
  }
  if(test == "z_test.pooled"){
    stats <- z_test.pooled(p_hat = p_hat, n = n, tail = tail, graph = graph)
  }
  if(test == "chi_squared.gof"){
    stats <- chi_squared.gof(obs_table = obs_table, expected_table = expected_table, expected_as_count = expected_as_count, row_totals = row_totals, graph = graph)
  }
  if(test == "chi_squared.ind"){
    stats <- chi_squared.ind(obs_table = obs_table, mat_totals = mat_totals, graph = graph)
  }
  if(test == "t_conf.two" | test == "t_conf.one"){
    stats <- find_conf(type = test, point = x_bar, sigma = sigma, level = level, n = n)
  }
  if(test == "z_conf.two" | test == "z_conf.one"){
    stats <- find_conf(type = test, point = p_hat, level = level, n = n)
  }

  return(stats)
}



