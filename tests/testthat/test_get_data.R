test_that("Testing the errors of the get_data function",{

  expect_error(get_data("potter"))
  expect_error(get_data("pot.csv"))
  expect_error(get_data("pot"))


})
