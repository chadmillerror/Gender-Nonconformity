test_that("no within group variability same sample size", {
  expect_error(welch_df(c(-1,-1,0,0), c(0,0,1,1)))
})
test_that("no within group variability diff sample size", {
  expect_error(welch_df(c(-1,0,0,0), c(0,1,1,1)))
})
test_that("equal variability, equal sample size", {
  expect_equal(welch_df(c(-1,-1,0,0), c(0,1,0,1)), 2)
})
test_that("equal variability, different sample size", {
  expect_equal(welch_df(c(-1,-1,0,0,-1,0), c(0,1,0,1,0,0)), 1.7142857)
})
test_that("equal variability, different sample size", {
  expect_error(welch_df(c(-1,-1,0,0,-1,0), c(0,1,0,1,0,0,2)))
})
test_that("equal variability, different sample size", {
  expect_error(welch_df(c(-1,-1,0,0,-1,0, "a"), c(0,1,0,1,0,0)))
})

