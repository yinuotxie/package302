# data for test
data("iris")
# input data frame
train <- iris[, -5]
# true class
cl <- iris[, 5]
test_that("my_knn_cv works when k_nn_1", {
  # predict classes when k_nn = 1
  my_pred_1 <- my_knn_cv(train, cl, k_nn = 1, k_cv = 5)
  # training test error
  train_error_1 <- sum(my_pred_1$class != cl)
  # expect training test error when k_nn = 1 is 0
  expect_equal(train_error_1, 0)
})

test_that("non numeric input for k_nn and k_cv throws error", {
  expect_error(my_knn_cv(train, cl, "a", "b"))
  expect_error(my_knn_cv(train, cl, 1, "b"))
  expect_error(my_knn_cv(train, cl, "a", 1))
})

test_that("my output is a list", {
  expect_is(my_knn_cv(train, cl, 1, 5), "list")
})


