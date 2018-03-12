h_upper_bound <- function(x){
  alpha0 <- -log(sqrt(2) - 1)
  
  A = 2 * x / alpha0 * 
        ( 
          sqrt( pi * alpha0 / (8*x) ) - 
          exp(alpha0 * x / 2) / ( x + sqrt(x^2 + 4 * x / alpha0) ) 
        )
  
  B = exp(-alpha0 * (floor(x) - 1) ^ 2 / (2*x) ) * 
          f_fraction(floor(x) - 0.5, x) / 
          (1 - f_fraction(floor(x) + 0.5, x))
  
  return(A + B)
}