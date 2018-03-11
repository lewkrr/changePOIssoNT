#' @title Skellam Likelihood ratio for varying window sizes
#'
#' @description Takes in a data frame of candidate changepoints and clique 
#'     capacities, then calculates the likelihood ratio for the difference
#'     of each clique.
#' 
#' @param in_data A dataframe containing the observed counts
#' @param candidates_and_reaches A dataframe containing the candidate 
#'     changepoints along with the maximum group sizes separated by their 
#'     respective changepoint.
#' 
#' @return 
#' @export


skellamLR = function(in_data,candidates_and_reaches){
  # Check that `candidates_and_reaches` is a dataframe
  if(!is.data.frame(candidates_and_reaches)){
    stop("candidates_and_reaches must be a dataframe with three columns")
  }

  candidate_cp  = as.numeric(candidates_and_reaches[1])
  left_reach = as.numeric(candidates_and_reaches[2])
  right_reach = as.numeric(candidates_and_reaches[3])

  if(length((candidate_cp - left_reach):(candidate_cp + right_reach - 1))==0) print(rownames(candidates_and_reaches),candidate_cp,left_reach,right_reach)
  x = in_data[(candidate_cp - left_reach):(candidate_cp + right_reach - 1)]

  x_l = in_data[(candidate_cp - left_reach):(candidate_cp-1)]
  x_r = in_data[(k):(k+right_reach-1)]

  s_l = sum(x_l)
  s_r = sum(x_r)
  s = sum(x)
  w = s_r - s_l

  xbar_l = s_l/left_reach
  xbar_r = s_r/right_reach
  xbar   = sum(x)/(left_reach+right_reach)

  options(warn=-1)
  like =  -xbar + xbar_r + xbar_l  - (w/2)*(log(xbar_r) - log(xbar_l)) + log(besselI(nu = w, x = 2*xbar)) - log(besselI(nu = w, x = 2*sqrt(xbar_l*xbar_r)))
  options(warn=0)

  if(xbar_l == xbar_r |  like == Inf | like == -Inf | is.na(like) | is.nan(like) ){
    like = NA
  }
  return(like)
}
