#' T-test
#'
#' Performs a one sample t-test on vectors of data.
#'
#' @param data A numeric vector of data.
#' @param mu A number indicating the null hypothesis value of the mean.
#' @param alt A character string specifying the alternative hypothesis, must be
#'   one of the "two.sided", "less", or "greater".
#'
#' @return A list with
#'   test_stat: the numeric test statistic,
#'   df: the degrees of freedom,
#'   alternative: the value of the parameter alternative,
#'   p_val: the numeric p-value.
#'
#' @examples
#' my_t.test(1:10, mu = 5, alt = "greater")
#' test <- rnorm(10, mean = 1, sd = 0)
#' my_t.test(test, 0, "two.sided")
#'
#' @export
my_t.test <- function(data, mu, alt) {
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
  output_list <- list("test_stat" = t,
                      "df" = df,
                      "p_val" = 0,
                      "alternative_hypothesis" = alt)

  # cacluate the p-value by pt()
  if (alt == "two.sided") {
    output_list$p_val <- pt(t, df, lower.tail = TRUE) * 2
    output_list$alternative_hypothesis <- "true difference in means is not 0"
  } else if (alt == "less") {
    output_list$p_val <- pt(t, df, lower.tail = TRUE)
    output_list$alternative_hypothesis <- "true difference in means is less than 0"
  } else {
    output_list$p_val <- pt(t, df, lower.tail = FALSE)
    output_list$alternative_hypothesis <- "true difference in means is greater than 0"
  }
  return(output_list)
}
