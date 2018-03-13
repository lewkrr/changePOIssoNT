#' @export


h_lower_bound <- function(x) {
  
  A =  2 * x * exp(-1 / x) /
      (1.5 + sqrt( 2.25 + 4*x) )
  
  # NOTE:  The below portion causes overflow.  Search for ways to get around this hurdle.
  B = 2 * x * exp(-(floor(x) + 1) * (floor(x) + 2) / (2 * x)) / 
    (floor(x) + 1.5 + sqrt((floor(x) + 1.5) ^ 2 + 8 * x / pi))
     
  C = exp( (floor(x) - 1) * floor(x) / (2*x) ) * 
    f_fraction(floor(x), x) * (1 + f_fraction(floor(x) + 1, x)) 
  
  return(A+B+C)
}
