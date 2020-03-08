# data for test
data("iris")
# input data frame
train <- iris[, -5]
# true class
cl <- iris[, 5]
test_that("my_rf_cv works", {
  # cv_err vector that stores cv_err for 100 trials
  cv_err <- rep(NA, 100)
  # mean mse for 100 trials
  for (i in 1:100) {
    cv_err[i] <- my_rf_cv(5)
  }
  mean <- mean(cv_err)
  sd <- sd(cv_err)
  my_result <- my_rf_cv(5)
  # I am satisified with 95 confidence interval, aka, my_result is 2sds aways
  # from the mean
  away <- (my_result - mean) / sd
  expect_true(abs(away) <= 2)
})

test_that("non numeric input for k throws an error", {
  expect_error(my_rf_cv("a"))
})
