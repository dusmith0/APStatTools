#' Title test()
#'
#' @param test
#' @param tail
#' @param null
#' @param x_bar
#' @param table_one
#' @param table_two
#' @param p_hat
#' @param sd
#' @param n
#' @param df
#' @param null_table
#' @param expected_table
#' @param expected_as_count
#' @param row_totals
#' @param mat_totals
#' @param data
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
#' # One sample test on means
#' test(test = "t_test.one", null = 10, x_bar = 11, sd = 3, n = 25, tail = "left", graph = TRUE)
#'
#' # Two sample test on means
#' test(test = "t_test.two", null = 0, x_bar = c(12,11), sd = c(2,3), n = c(25,25), tail = "left", graph = TRUE)
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
#' test(test = 'chi_squared.gof', null_table = X, expected_table = Y, expected_as_count = FALSE, row_totals = FALSE, graph = TRUE)
#'
#' # This vector included a total at the end, and has the expected table as a count instead of frequency.
#' # Note the function will assume that both expected an null contain totals.
#' X <- c(10,13,14,20,16,73)
#' Y <- c(8,12,8,16,13,57)
#' test(test = 'chi_squared.gof', null_table = X, expected_table = Y, expected_as_count = TRUE, row_totals = TRUE, graph = TRUE)
#'
#' # Chi-squared test for independence
#' X <- matrix(rnorm(20,10,3), nrow = 4)
#' test(test = 'chi_squared.ind', null_table = X, mat_totals = FALSE, graph)
#'
test <- function(test = "help", tail="left", null, x_bar, table_one, table_two, p_hat, sd, n, df,
                 null_table = NULL, expected_table = NULL, expected_as_count = FALSE, row_totals = FALSE, mat_totals = FALSE,
                 data=NULL,graph=TRUE){

  ## This is to allow the user to see what is needed for each test type.
  if(test == "help"){
    stop(paste("Below is a list of the needed inputs for each test type:

               ('t_test.one', null, x_bar, sd, n, tail, graph)
               ('t_test.two', null, x_bar = c(1,2), sd, n tail, graph)
               ('t_test.paired', null, table_one, table_two, tail, graph)
               ('z_test.one', null, p_hat, n, tail, graph)
               ('z_test.pooled', p_hat, n, tail, graph)
               ('chi_squared.gof', null_table, expected_table, expected_as_count, row_totals, graph)
               ('chi_squared.int', null_table, mat_totals, graph)
               "))
  }
  if(test == "t_test.one"){
    stats <- t_test_one(null = null, x_bar = x_bar, sd = sd, n = n, tail = tail, graph = graph)
  }
  if(test == "t_test.two"){
    stats <- t_test_two(null = null, x_bar = x_bar, sd = sd, n = n, tail = tail, graph = graph)
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
    stats <- chi_squared.gof(null_table = null_table, expected_table = expected_table, expected_as_count = expected_as_count, row_totals = row_totals, graph = graph)
  }
  if(test == "chi-squared.ind"){
    stats <- chi_squared.ind(null_table = null_table, mat_totals = mat_totals, graph = graph)
  }

  return(stats)
}


tests <- list("t_test","t_test.paired","z_test.one","z_test.two","z-test.pooled","chi-squared","ANOVA",
              "chi-squared.GOF","chi-squared.homogeneity","chi-squared.independence")

tail <- list("left","right","two")
