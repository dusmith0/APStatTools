test_that("Testing the errors of the build_dist function",{

  expect_error(build_dist(display_prob = TRUE))
  expect_error(build_dist("Normal"))
  expect_error(build_dist("N"))
  expect_error(build_dist(N))
  expect_error(build_dist(tail = "l"))
  expect_error(build_dist(tail = "rightt"))
  expect_error(build_dist('help'))

})
