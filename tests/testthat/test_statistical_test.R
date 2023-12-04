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



