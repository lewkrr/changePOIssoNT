


lower_H_bound <- function(nu,x) {
  alpha0 = -log(sqrt(2) - 1)
  
  A =  2 * x * exp(-(nu + 1) / x) /
      (nu + 1.5 + sqrt((nu + 1.5)^2 + 4 * x) )
  
  # NOTE:  The below portion causes overflow.  Search for ways to get around this hurdle.
  B = 2 * x * exp(-(floor(x) - nu + 1) * (floor(x) + nu + 2) / (1000 *2 * x)) / 
    (floor(x) + 1.5 + sqrt((floor(x) + 1.5) ^ 2 + 8 * x / pi))
     
  C = exp( (floor(x) - floor(nu) - 1) * (floor(x) + nu  )) * 
    f_fraction(floor(x), x) * f_fraction(floor(x) + 1, x) 
  
  A+B+C
  
  
   
}
