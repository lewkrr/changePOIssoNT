upper_LR_bound <- function (w, left_avg, right_avg, null_avg) {
  alpha0 <- -log(sqrt(2) - 1)
  
  x = 2 * null_avg
  
  y = 2 * sqrt(left_avg * left_avg)
  
  log_upper_bound <- gamma(floor(x) + x + 0.5) / gamma(floor(y) + y/2 + 1) *
         w ^ (-x + y/2 + 0.5) * 
         y ^ floor(y) / x ^ floor(x) *
         (1 + 2 * h_upper_bound(y)) / (1 + 2 * h_lower_bound(x)) *
         (2 * right_avg / null_avg ) ^ w *
         exp( 
              floor(y) * (floor(y) + 1) / (2*y)  -
                 alpha0 * floor(x)^2 / (2*x) -
                 (right_avg + left_avg)
           )
  
  return(upper_bound)
}