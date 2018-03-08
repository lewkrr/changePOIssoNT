#' @title Skellam Likelihood ratio for varying window sizes
#'
#' @description Takes in a data frame of changepoints and window sizes, then calculates the LR for each
#' 
#' @param in.data a dataframe containing the observed counts
#' @param windows A dataframe containing the candidate changepoints along with the maximum possible values for each window
#' 
#' @return 
#' @export


skellamLR = function(in.data,windows){

  k  = as.numeric(windows[1])
  jl = as.numeric(windows[2])
  jr = as.numeric(windows[3])

  if(length((k-jl):(k+jr-1))==0) print(rownames(windows),k,jl,jr)
  x = in.data[(k-jl):(k+jr-1)]

  x_l = in.data[(k-jl):(k-1)]
  x_r = in.data[(k):(k+jr-1)]

  s_l = sum(x_l)
  s_r = sum(x_r)
  s = sum(x)
  w = s_r - s_l

  xbar_l = s_l/jl
  xbar_r = s_r/jr
  xbar   = sum(x)/(jl+jr)

  options(warn=-1)
  like =  -xbar + xbar_r + xbar_l  - (w/2)*(log(xbar_r) - log(xbar_l)) + log(besselI(nu = w, x = 2*xbar)) - log(besselI(nu = w, x = 2*sqrt(xbar_l*xbar_r)))
  options(warn=0)

  if(xbar_l == xbar_r |  like == Inf | like == -Inf | is.na(like) | is.nan(like) ){
    like = NA
  }
  return(like)
}
