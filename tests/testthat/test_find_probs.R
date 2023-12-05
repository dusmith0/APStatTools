test_that("That the find_probs function is producing the correct values"{

  expect_equal(find_probs(1.5),pnorm(1.5,0,1))
  expect_equal(find_probs(1.5,mean = 12, sd = 5),pnorm(1.5,12,5))
  expect_equal(find_probs(c(-3,-2,-1,0,1,2,3)), c(pnorm(-3,0,1),pnorm(-2,0,1),pnorm(-1,0,1),
                pnorm(0,0,1),pnorm(1,0,1),pnorm(2,0,1),pnorm(3,0,1)))
  expect_equal(find_probs(c(-1,3),type="chi-squared",tail="inner",df=4),
               (pchisq(3,4) - pchisq(-1,4)))
  expect_equal(find_probs(c(-1,3),type="t-dist",tail="right",df=4) ,
               pt(c(-1,3), 4, lower.tail = FALSE))
  expect_equal(find_probs(c(2,6),type="binomial",tail="two"),
               sum(pbinom(2, 10, .5)) + pbinom(6, 10, .5, lower.tail = FALSE))
  expect_equal(find_probs(c(2,6),type="binomial",tail="outer"),
               sum(pbinom(2, 10, .5)) + pbinom(6, 10, .5, lower.tail = FALSE))
  expect_equal(find_probs(4,type="binomial",tail="left"),
               (pbinom(4, 10, .5)))
  expect_equal(find_probs(4,type="binomial",tail="left_not_equal"),
               (pbinom(3, 10, .5)))
  expect_equal(find_probs(bound = .4, inverse = TRUE), qnorm(.4,0,1))
  expect_equal(find_probs(bound = .8, mean = 10, sd = 4, inverse = TRUE), qnorm(.8,10,4))

})
