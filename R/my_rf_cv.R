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
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k) {
  # randomly assigns observations to folds 1,…,k with equal probability
  fold <- sample(rep(1:k, length = nrow(train)))
  data <- my_gapminder[, -1] %>% mutate(fold = fold)

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
    model <- randomForest(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
                          , data = data_train, ntree = 100)
    # predict the Sepal.Length of test data
    # the first column of data_test is Sepal.Length
    pred <- predict(model, data_test[, -1])
    # evaluate MSE, the average squared difference between predicted
    # Sepal.Length and true Sepal.Length.
    mse[i] <- mean((pred - data_test[, 1])^2)
  }

  # return the average mse across all k folds
  return(mean(mse))
}
