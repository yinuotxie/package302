#' Random Forest Cross-Validation
#'
#' Predicts outputs Sepal.Length using covariates
#' Sepal.Width, Petal.Length, and Petal.Width by random forest methods.
#'
#' @param k Number of folds.
#' @keywords cross-validation
#'
#' @return A numeric with the cross-validation error
#'
#' @import class
#' @import randomForest
#' @import magrittr
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr contains
#' @importFrom stats predict
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  data("my_gapminder", envir = environment())
  train <- my_gapminder %>% select(lifeExp, gdpPercap)
  # randomly assigns observations to folds 1,…,k with equal probability
  fold <- sample(rep(1:k, length = nrow(train)))
  data <- train %>% mutate(fold = fold)

  # evaluate MSE, the average squared difference between predicted
  # Sepal.Length and true Sepal.Length.
  mse <- rep(NA, k)
  # loop through the folds
  for (i in 1:k) {
    # get the training data
    data_train <- data %>% filter(fold != i) %>% select(-contains("fold"))
    # get the test data
    data_test <- data %>% filter(fold == i) %>% select(-contains("fold"))
    # get the model
    model <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 100)
    # predict the life expentance of test data
    # the first column of data_test is Sepal.Length
    pred <- predict(model, data_test$gdpPercap)
    # evaluate MSE, the average squared difference between predicted
    # lifeExp and true lifeExp
    mse[i] <- mean((pred - data_test$lifeExp)^2)
  }

  # return the average mse across all k folds
  return(mean(mse))
}
