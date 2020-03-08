# test data
data(mtcars)
# test my result
object <- mpg ~ hp + wt
test <- lm(formula = object, data = mtcars)

test_that("my_lm works", {
  result <- summary(test)
  my_result <- my_lm(object, data = mtcars)

  result_matr <- as.matrix(result$coefficients[, -4]) %>% round(5)
  my_result_matr <- as.matrix(my_result[, -4]) %>% round(5)
  # check whether the estimate, se, and t.value equal to the real ones
  expect_equivalent(result_matr, my_result_matr)
})

