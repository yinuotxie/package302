test <- rnorm(10, mean = 0, sd = 1)

test_that("my_t_test works for two-sided", {
  result <- t.test(test, mu = 0, alternative = "two.sided")
  my_result <- my_t_test(test, 0, "two.sided")
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative_hypothesis, "true mean is not equal to 0")
})

test_that("my_t_test works for less", {
  result <- t.test(test, mu = 0, alternative = "less")
  my_result <- my_t_test(test, 0, "less")
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative_hypothesis, "true mean is less than 0")
})

test_that("my_t_test works for greater", {
  result <- t.test(test, mu = 0, alternative = "greater")
  my_result <- my_t_test(test, 0, "greater")
  expect_true(my_result$test_stat == round(result$statistic, 5))
  expect_true(my_result$p_val == round(result$p.value, 4))
  expect_true(my_result$df == result$parameter)
  expect_match(my_result$alternative_hypothesis, "true mean is greater than 0")
})

test_that("my_t_test throws an error when input requirements aren't meet", {
  expect_error(my_t_test(test, 0, "a"))
})

test_that("the output is a list", {
  expect_is(my_t_test(test, 0, "two.sided"), "list")
})
