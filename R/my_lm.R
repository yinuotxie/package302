#' Fitting linear models
#'
#' my_lm is used to fit linear models.
#'
#' @param formula A class object
#' @param data A set of data
#' @keywords prediction
#'
#' @return A dataframe of summary with rows for each coefficient and columns for the
#'   Estimate, Std. Error, t value, and Pr(>|t|).
#'
#' @examples
#' my_lm(mpg ~ hp + wt, mtcars)
#'
#' @import magrittr
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @importFrom stats model.response
#' @importFrom stats pt
#'
#' @export
my_lm <- function(formula, data) {

  # extract all the objects in the formula
  model <- model.frame(formula = formula, data = data)
  # extract all independent variable
  x <- model.matrix(object = formula, data = data)
  # extract all dependent variable
  y <- as.matrix(model.response(model))

  # get the coefficient
  coef <- solve(t(x) %*% x) %*% t(x) %*% y

  # get the degree of freedom
  # df = n - # of parameter - 1
  # # of parameter  = ncol(x) + 1
  df <- nrow(x) - ncol(x)

  # get the variance
  var <- sum((y - x %*% coef)^2 / df)

  # get the standard error of each variable
  se <- sqrt(var * solve(t(x) %*% x)) %>%
    diag() %>%
    as.matrix()

  # get the t-value
  t_value <- ((coef - 0) / se) %>% round(5)

  # get Pr(>|t|)
  pr <- pt(abs(t_value), df, lower.tail = FALSE) * 2

  # round coef, se, t_value to 5 decimals to display
  coef <- round(coef, 5)
  se <- round(se, 5)
  t_value <- round(t_value, 5)

  # combine all the information to one single matrix
  result <- cbind(coef, se, t_value, pr)
  colnames(result) <- c("Estimate", "Std.Error", "t.value", "Pr(>|t|)")

  # convert the result to a data frame in order to display as a table
  result <- data.frame(result)
  return(result)
}
