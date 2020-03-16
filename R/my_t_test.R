#' T-test
#'
#' Performs a one sample t-test on vectors of data.
#'
#' @param data A numeric vector of data.
#' @param mu A number indicating the null hypothesis value of the mean.
#' @param alt A character string specifying the alternative hypothesis, must be
#'   one of the "two.sided (default)", "less", or "greater".
#' @keywords inference
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item test_stat: the numeric test statistic;
#'   \item df: the degrees of freedom;
#'   \item alternative: the value of the parameter alternative;
#'   \item p_val: the numeric p-value.
#' }
#'
#' @examples
#' my_t_test(1:10, mu = 5, alt = "greater")
#'
#' # my_t_test on a normal distribution data
#' test <- rnorm(10, mean = 1, sd = 0)
#' my_t_test(test, 0, "two.sided")
#'
#' @importFrom stats sd
#' @importFrom stats pt
#'
#' @export
my_t_test <- function(data, mu, alt = "two.sided") {

  # generate errors if  input alt does not meet the requirments
  if (alt != "two.sided" & alt != "less" &  alt != "greater") {
    stop("alternative must be \"two.sided\", \"less\", or \"greater\"")
  }

  # calculate the mean and se of the data
  data_mu <- mean(data)
  data_se <- sd(data) / sqrt(length(data))

  # calculate the t value
  t <- (data_mu - mu) / data_se

  # degree of freedom
  df <- length(data) - 1

  # create a default output list
  # round t value to 5 decimals
  output_list <- list("test_stat" = round(t, 5),
                      "df" = df,
                      "p_val" = 0,
                      "alternative_hypothesis" = alt)

  # caculate the p-value by pt()
  # round all the p_values to 4 decimals
  if (alt == "two.sided") {
    p_val <- abs(pt(t, df, lower.tail = FALSE)) * 2
    # if p_vale cannot greater than 1
    if (p_val > 1) {
      p_val <- 2 - p_val
    }
    output_list$p_val <- round(p_val, 4)
    output_list$alternative_hypothesis <- paste0("true mean is not equal to ", mu)
  } else if (alt == "less") {
    output_list$p_val <- round(pt(t, df, lower.tail = TRUE), 4)
    output_list$alternative_hypothesis <- paste0("true mean is less than ", mu)
  } else {
    output_list$p_val <- round(pt(t, df, lower.tail = FALSE), 4)
    output_list$alternative_hypothesis <- paste0("true mean is greater than ", mu)
  }
  return(output_list)
}
