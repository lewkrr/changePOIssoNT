


lower_LR_bound <- function (w, left_avg, right_avg, null_avg) {
  alpha0 <- -log(sqrt(2) - 1)
  
  x = 2 * null_avg
  
  y = 2 * sqrt(left_avg * left_avg)
  
  low_bound <- gamma(floor(x) + x/2 + 1) / gamma(floor(y) + y + 0.5) * 
    w ^ (y - x/2 - 0.5) *
    y ^ floor(y) / x ^ floor(x) *
    (1 + 2 * h_lower_bound(y)) / (1 + 2 * h_upper_bound(x)) *
    (0.5 * null_avg / left_avg) ^ w *
    exp( 
      - floor(x) * (floor(x) + 1) / (2*x)  +
        alpha0 * floor(y)^2 / (2*y) -
        (right_avg + left_avg)
    )
  
  return(low_bound)
}