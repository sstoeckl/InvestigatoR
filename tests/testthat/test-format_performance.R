test_that("performance works with NA", {
  expect_equal(performance(c(1,2,NA,3)),2)
})

