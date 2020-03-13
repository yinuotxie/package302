data("my_gapminder", envir = environment())
my_gapminder <- my_gapminder
gdpPercap <- NULL
lifeExp <- NULL
#' Random Forest Cross-Validation
#'
#' Predicts outputs lifeExp using covariates gdpPercap by random forest methods.
#'
#' @param k Number of folds.
#' @keywords cross-validation
#'
#' @return A numeric with the cross-validation error.
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
  # generates an error if the input k is not numeric
  if (!is.numeric(k)) {
    stop("K must be numeric")
  }
  # randomly assigns observations to folds 1,…,k with equal probability
  train <- my_gapminder %>% select(lifeExp, gdpPercap)
  fold <- sample(rep(1:k, length = nrow(train)))
  my_data <- train %>% mutate(fold = fold)

  # evaluate MSE, the average squared difference between predicted
  # Sepal.Length and true Sepal.Length.
  mse <- rep(NA, k)
  # loop through the folds
  for (i in 1:k) {
    # get the training data
    data_train <- my_data %>% filter(fold != i) %>% select(-fold)
    # get the test data
    data_test <- my_data %>% filter(fold == i) %>% select(-fold)
    # get the model
    model <- randomForest(lifeExp ~ gdpPercap, data = data_train, ntree = 50)
    # predict the life expentance of test data
    # the second column is gdpPercap
    pred <- predict(model, data_test[, 2])
    # evaluate MSE, the average squared difference between predicted
    # lifeExp and true lifeExp
    mse[i] <- mean((pred - data_test$lifeExp)^2)
  }

  # return the average mse across all k folds
  return(mean(mse))
}
