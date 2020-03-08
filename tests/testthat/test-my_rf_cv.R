test_that("non numeric input for k throws an error", {
  expect_error(my_rf_cv("a"))
})
