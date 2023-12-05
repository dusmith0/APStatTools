test_that("test preforms the one sample t-test correclty", {
  found <- test(test = "t_test.one", null = 10, x_bar = 8, sd = 3, n = 25, tail = "left", graph = FALSE)
  null <- 10
  x_bar <- 8
  sd <- 3
  n <- 25

  expect_equal(found$n, 25)
  expect_equal(found$standard_error, sd/sqrt(n))
  expect_equal(found$df, n - 1)
  expect_equal(found$test_statistic, (8 - 10)/(3/sqrt(25)))
  expect_equal(found$p_value, pt((8 - 10)/(3/sqrt(25)),25-1))

  #Testing for right tails
  found <- test(test = "t_test.one", null = 10, x_bar = 8, sd = 3, n = 25, tail = "right", graph = FALSE)
  expect_equal(found$p_value, pt((8 - 10)/(3/sqrt(25)),25-1, lower.tail = FALSE))

  #testing for two tails
  found <- test(test = "t_test.one", null = 10, x_bar = 8, sd = 3, n = 25, tail = "two", graph = FALSE)
  expect_equal(found$p_value, pt((8 - 10)/(3/sqrt(25)),25-1) * 2)

})


test_that("test preforms the two sample t-test correclty", {
  found <- test(test = "t_test.two", null = 0, x_bar = c(12,9), sd = c(2,3), n = c(25,30), tail = "left", graph = FALSE)
  null <- 0
  x_bar <- c(12,9)
  sd <- c(2,3)
  n <- c(25,30)
  diff(-x_bar)

  expect_equal(found$n, c(25,30))
  expect_equal(found$standard_error, c(sd[1]/sqrt(n[1]), sd[2]/sqrt(n[2])))
  expect_equal(round(found$df,3), round(c(50.742,50.742),3))
  expect_equal(found$test_statistic[1], (12 - 9)/(sqrt( sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2])))
  expect_equal(found$p_value[1], pt((12 - 9)/(sqrt( sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2])),50.742))

  #Testing for right tails
  found <- test(test = "t_test.two", null = 0, x_bar = c(12,9), sd = c(2,3), n = c(25,30), tail = "right", graph = FALSE)
  expect_equal(found$p_value[1], pt((12 - 9)/(sqrt( sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2])),50.742007,lower.tail = FALSE))

  #testing for two tails
  found <- test(test = "t_test.two", null = 0, x_bar = c(12,9), sd = c(2,3), n = c(25,30), tail = "two", graph = FALSE)
  expect_equal(found$p_value[1], pt((12 - 9)/(sqrt( sd[1] ^ 2 / n[1] + sd[2] ^ 2 / n[2])),50.742007,lower.tail = TRUE) * 2)

})

test_that("test preforms the one sample z-test correclty", {
 found <- test(test = 'z_test.one', null = .4, p_hat = .6, n = 10, tail = "left", graph = FALSE)

 expect_equal(found$n, 10)
 expect_equal(found$standard_error, sqrt(.4 *(1 - .4)/10))
 expect_equal(found$test_statistic, (.6 - .4)/sqrt(.4 *(1 - .4)/10))
 expect_equal(found$p_value, pnorm((.6 - .4)/sqrt(.4 *(1 - .4)/10),0,1))

 #Testing for right tails
 found <- test(test = 'z_test.one', null = .4, p_hat = .6, n = 10, tail = "right", graph = FALSE)
 expect_equal(found$p_value, pnorm((.6 - .4)/sqrt(.4 *(1 - .4)/10),0,1,lower.tail=FALSE))

 #testing for two tails
 found <- test(test = 'z_test.one', null = .4, p_hat = .6, n = 10, tail = "two", graph = FALSE)
 expect_equal(found$p_value, pnorm((.6 - .4)/sqrt(.4 *(1 - .4)/10),0,1,lower.tail=TRUE)*2)

})


test_that("test preforms the two sample z-test correclty", {
  found <- test(test = 'z_test.pooled', p_hat = c(.4,.6), n = c(20,30), tail = "left", graph = FALSE)

  expect_equal(found$n, c(20,30))
  pc <- (.4*(20) + .6*(30))/50
  expect_equal(found$standard_error[1], sqrt(pc*(1 - pc)*(1/20 + 1/30)))
  expect_equal(found$test_statistic[1], (.4 - .6)/sqrt(pc*(1 - pc)*(1/20 + 1/30)))
  expect_equal(found$p_value[1], pnorm((.4 - .6)/sqrt(pc*(1 - pc)*(1/20 + 1/30))))

  #Testing for right tails
  found <- test(test = 'z_test.pooled', p_hat = c(.4,.6), n = c(20,30), tail = "right", graph = FALSE)
  expect_equal(found$p_value[1], pnorm((.4 - .6)/sqrt(pc*(1 - pc)*(1/20 + 1/30)),lower.tail = FALSE))

  #testing for two tails
  found <- test(test = 'z_test.pooled', p_hat = c(.4,.6), n = c(20,30), tail = "two", graph = FALSE)
  expect_equal(found$p_value[1], pnorm((.4 - .6)/sqrt(pc*(1 - pc)*(1/20 + 1/30)),lower.tail = TRUE) * 2)

})


test_that("test preforms the chi-squared GOF correclty", {
   X <- c(10,13,14,20,16)
   Y <- c(.2,.2,.2,.2,.2)

   found <- test(test = 'chi_squared.gof', obs_table = X, expected_table = Y, expected_as_count = FALSE, row_totals = FALSE, graph = FALSE)
   expect_equal(found$obs_table, X)
   expect_equal(found$expected_count, rep(.2 * 73, 5))
   expect_equal(found$df[1], 4)
   expect_equal(found$chi_squared_values, (X - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5))
   expect_equal(found$test_statistic[1], sum((X - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5)))
   expect_equal(found$p_value[1], pchisq(sum((X - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5)), 4, lower.tail = FALSE))

   #Checking when the total is given.
   x <- c(10,13,14,20,16,73)
   y <- c(.2,.2,.2,.2,.2,1)

   found <- test(test = 'chi_squared.gof', obs_table = x, expected_table = y, expected_as_count = FALSE, row_totals = TRUE, graph = FALSE)
   expect_equal(found$obs_table, c(10,13,14,20,16))
   expect_equal(found$expected_count, rep(.2 * 73, 5))
   expect_equal(found$df[1], 4)
   expect_equal(found$chi_squared_values, (X[-6] - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5))
   expect_equal(found$test_statistic[1], sum((X[-6] - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5)))
   expect_equal(found$p_value[1], pchisq(sum((X[-6] - rep(.2 * 73, 5))^2 / rep(.2 * 73, 5)), 4, lower.tail = FALSE))


   #Checking when the total is given, and count is given.
   X <- c(10,13,14,20,16,73)
   Y <- c(8,12,8,16,13,57)

   found <- test(test = 'chi_squared.gof', obs_table = X, expected_table = Y, expected_as_count = TRUE, row_totals = TRUE, graph = FALSE)
   expect_equal(found$obs_table, c(10,13,14,20,16))
   expect_equal(found$expected_count, c(8,12,8,16,13))
   expect_equal(found$df[1], 4)
   expect_equal(found$chi_squared_values, (X[-6] - c(8,12,8,16,13)) ^ 2 / c(8,12,8,16,13))
   expect_equal(found$test_statistic[1], sum((X[-6] - c(8,12,8,16,13))^2 / c(8,12,8,16,13)))
   expect_equal(found$p_value[1], pchisq(sum((X[-6] - c(8,12,8,16,13))^2 / c(8,12,8,16,13)), 4, lower.tail = FALSE))

})

test_that("test preforms the chi-squared Independence test correclty", {
   set.seed(100)
   X <- matrix(sample(1:20,15), nrow = 3)
   found <- test(test = 'chi_squared.ind', obs_table = X, mat_totals = FALSE, graph = FALSE)

   expect_equal(found$obs_table[1:3,1:5], (X))
   expect_table <- matrix(c(14.064,14.503,17.580,13.185,9.669,11.414,11.771,14.268,
                            10.701,7.847,6.522,6.726,8.153,6.115,4.484)
                          ,nrow = 3, byrow = TRUE )
   expect_equal(round(found$expected_count[1:3,1:5],3), expect_table)
})
