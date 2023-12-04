test_that("get_measures computes correct values on a single vector", {
  X <- c(1,2,3,4,5)
  found <- get_measures(X = X, outliers = TRUE)
  expect_equal(found$measures[[2]], sum(X))
  expect_equal(found$measures[[3]], length(X))
  expect_equal(found$measures[[4]], mean(X))
  expect_equal(found$measures[[6]], sd(X))
  expect_equal(found$measures[[5]], sqrt((sum((X - mean(X))^2)/length(X))))
  expect_equal(found$measures[[7]], 1.5)
  expect_equal(found$measures[[8]], 3)
  expect_equal(found$measures[[9]], 4.5)
  expect_equal(found$measures[[10]], (4.5 - 1.5))
  expect_equal(found$measures[[11]], -3)
  expect_equal(found$measures[[12]], 9)

  found <- get_measures(X = X, outliers = "3sd Rule")
  expect_equal(found$measures[[11]], mean(X) - 3 * sd(X))
  expect_equal(found$measures[[12]], mean(X) + 3 * sd(X))
  })


test_that("get_measures computes correct values on a matrix of 3x5", {
  X <- matrix(c(1,2,3,4,5,10,11,12,13,14,100,200,300,400,500), ncol = 3)
  found <- get_measures(X = X, outliers = TRUE)
  expect_equal(found$measures[[2]], colSums(X))
  expect_equal(found$measures[[3]][1], nrow(X))
  expect_equal(found$measures[[4]], colMeans(X))
  expect_equal(found$measures[[6]], c(sd(X[,1]),sd(X[,2]),sd(X[,3])))
  expect_equal(found$measures[[5]], c(sqrt((sum((X[,1] - mean(X[,1]))^2)/nrow(X))) ,
                                    sqrt((sum((X[,2] - mean(X[,2]))^2)/nrow(X))),
                                    sqrt((sum((X[,3] - mean(X[,3]))^2)/nrow(X)))))
  expect_equal(found$measures[[7]], c(1.5, 10.5, 150))
  expect_equal(found$measures[[8]], c(3, 12, 300))
  expect_equal(found$measures[[9]], c(4.5, 13.5, 450))
  expect_equal(found$measures[[10]], c(3,3,300))
  expect_equal(found$measures[[11]], c(-3, 6, -300))
  expect_equal(found$measures[[12]], c(9, 18, 900))

  found <- get_measures(X = X, outliers = "3sd Rule")
  expect_equal(found$measures[[11]], c(mean(X[,1]) - 3 * sd(X[,1]),
                                       mean(X[,2]) - 3 * sd(X[,2]),
                                       mean(X[,3]) - 3 * sd(X[,3])))
  expect_equal(found$measures[[12]], c(mean(X[,1]) + 3 * sd(X[,1]),
                                       mean(X[,2]) + 3 * sd(X[,2]),
                                       mean(X[,3]) + 3 * sd(X[,3])))
})


test_that("get_measures computes correct on a large random data set of 1120X3.
          Note: for this data set, calculations checked by hand were ignored.", {
  set.seed(12)
  X <- matrix(rnorm(3360,0,25), ncol = 3)
  found <- get_measures(X = X, outliers = TRUE)
  expect_equal(found$measures[[2]], colSums(X))
  expect_equal(found$measures[[3]][1], nrow(X))
  expect_equal(found$measures[[4]], colMeans(X))
  expect_equal(found$measures[[6]], c(sd(X[,1]),sd(X[,2]),sd(X[,3])))
  expect_equal(found$measures[[5]], c(sqrt((sum((X[,1] - mean(X[,1]))^2)/nrow(X))) ,
                                      sqrt((sum((X[,2] - mean(X[,2]))^2)/nrow(X))),
                                      sqrt((sum((X[,3] - mean(X[,3]))^2)/nrow(X)))))

  found <- get_measures(X = X, outliers = "3sd Rule")
  expect_equal(found$measures[[11]], c(mean(X[,1]) - 3 * sd(X[,1]),
                                       mean(X[,2]) - 3 * sd(X[,2]),
                                       mean(X[,3]) - 3 * sd(X[,3])))
  expect_equal(found$measures[[12]], c(mean(X[,1]) + 3 * sd(X[,1]),
                                       mean(X[,2]) + 3 * sd(X[,2]),
                                       mean(X[,3]) + 3 * sd(X[,3])))
})

test_that("get_measures computes correct on a regression.", {
  X <- c(1,2,3,4,5)
  Y <- c(10,20,30,40,50) + rnorm(5,0,1)
  found <- get_measures(X = X, Y = Y, outliers = TRUE, regression = TRUE)
  expect_equal(found$measures[[2]], c(sum(X),sum(Y)))
  expect_equal(found$measures[[3]], c(length(X),length(Y)))
  expect_equal(found$measures[[4]], c(mean(X), mean(Y)))
  expect_equal(found$measures[[6]], c(sd(X),sd(Y)))
  expect_equal(found$measures[[5]], c(sqrt((sum((X - mean(X))^2)/length(X))),
                                      sqrt((sum((Y - mean(Y))^2)/length(Y)))))

  found <- get_measures(X = X, Y = Y, outliers = "3sd Rule", regression = TRUE)
  expect_equal(found$measures[[11]], c(mean(X) - 3 * sd(X),
                                     mean(Y) - 3 * sd(Y)))
  expect_equal(found$measures[[12]], c(mean(X) + 3 * sd(X),
                                     mean(Y) + 3 * sd(Y)))
})

