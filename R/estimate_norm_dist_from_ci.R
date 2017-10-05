
#' A Function to Estimate a Normal Distribution from Credible or Confidence Intervals
#'
#' @param lower_interval Numeric, the lower CI.
#' @param upper_interval Numeric, the upper CI
#' @param interval A character string indicating the percentage interval the CI represents. Defaults to "95\%".
#' @return A dataframe containing the mean and standard deviation of the normal distribution summarised by the provided CI's.
#' @export
#' @importFrom tibble data_frame
#' @examples
#'
#' ## Run function to estimate normal distribution for a 95% CI of 1, to 2
#'
#' df <- estimate_norm_dist_from_ci(1,2)
#'
#' ## Check
#'
#' x <- rnorm(10000, df$mean, df$sd)
#'
#' quantile(x, c(0.025, 0.975))
estimate_norm_dist_from_ci <- function(lower_interval = NULL,
                                                       upper_interval = NULL,
                                                       interval = "95%") {

if (!(interval %in% "95%")) {
  stop("Only a 95% credible interval has been implemented, please fork the package on github and add your required interbal")
}
  z <- 1.96

  mean <- lower_interval + (upper_interval - lower_interval)/2
  sd <- (upper_interval - mean)/z

  out <- tibble::data_frame(mean = mean, sd = sd)
  return(out)
}
