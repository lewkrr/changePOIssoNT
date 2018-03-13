#' @title Bound on the skellam likelihood ratio.
#' 
#' @description Because the modiffied bessel function fails for large indices, this
#'     function applied the findings of 
#' 
#' @param w 
#'     A numeric of length 1.  The osverved value of the Skellam random variable.
#' @param left_avg 
#'     The estimate of the intensity parameter for the observations within
#'     the left group.
#' @param right_avg 
#'     The estimate of the intensity parameter for the observations within
#'     the right group.
#' @param null_avg
#'     The estimate of the intensity parameter for the observations within
#'     the combined left and right groups.
#' @param lower_bound
#'   Logical.  Determines whether to calculate the lower of the upper bounds of 
#'   the skellam likelihood ratio.
#'     
#' @export
#' 
#' @include h_upper_bound.R
#' @include h_lower_bound.R
#' 
#' @importFrom Rdpack reprompt
#' 
#' @references
#' \insertRef{balachandran2013exponential}{changePOIssoNT}

skellam_LR_bound <- function (w = NULL, 
                      left_avg = NULL, right_avg = NULL, 
                      null_avg = NULL, 
                      lower_bound = TRUE) {
  alpha0 <- -log(sqrt(2) - 1)
  x = 2 * null_avg
  y = 2 * sqrt(left_avg * left_avg)
  
  if (lower_bound) {
    low_log_bound <-
      - (floor(x)^2 + floor(x)) / (2*x) + alpha0 * floor(y)^2 / (2*y) +
      lgamma(floor(x) + x/2 +1) - lgamma(floor(y) + y + 1/2) +
      (y - x/2 - 0.5) * log(w) + w * (log(x) - log(y) - log(2))  +
      floor(y) * log(y) - floor(x) * (log(x) - log(2)) +
      log(1 + 2 * h_lower_bound(y)) - log(1 + 2 * h_upper_bound(x)) +
      (w/2) * (log(right_avg) - log(left_avg)) -
      (right_avg + left_avg)
    return(low_log_bound)
  } else {
    log_upper_bound <-
      (floor(y)^2 + floor(y)) / (2*y) - alpha0 * floor(x)^2 / (2*x) -
      lgamma(floor(y) + y/2 +1) + lgamma(floor(x) + x + 1/2) +
      (-x + y/2 + 0.5) * log(w) + w * (-log(y) + log(x) + log(2)) +
      - floor(x) * log(x) + floor(y) * (log(y) - log(2)) +
      - log(1 + 2 * h_lower_bound(x)) + log(1 + 2 * h_upper_bound(y)) +
      (w/2) * (log(right_avg) - log(left_avg)) -
      (right_avg + left_avg)
    return(log_upper_bound)
  }
}