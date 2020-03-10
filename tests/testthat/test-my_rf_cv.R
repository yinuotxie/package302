test_that("output is numeric", {
  expect_is(my_rf_cv(5), "numeric")
})

test_that("non numeric input for k throws an error", {
  expect_error(my_rf_cv("a"))
})
