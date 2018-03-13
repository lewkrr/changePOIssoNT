#' @title Skellam Likelihood ratio for varying group sizes
#'
#' @description Takes in a data frame of candidate changepoints and clique 
#'     capacities, then calculates the likelihood ratio for the difference
#'     of each clique.
#' 
#' @param in_counts A dataframe containing the observed counts
#' @param left_and_right_reaches A dataframe containing the candidate 
#'     changepoints along with the maximum group sizes separated by their 
#'     respective changepoint.
#' 
#' @return 
#' @export


skellamLR = function(contender_chng_pt, left_and_right_reaches, in_counts){
  # FIXME add checks for inputs
  stopifnot(is.numeric(contender_chng_pt) | is.integer(contender_chng_pt))
  stopifnot(is.data.frame(left_and_right_reaches))
  stopifnot(is.numeric(in_counts))
  
  left_reach <- left_and_right_reaches[["left_reaches"]]
  right_reach <- left_and_right_reaches[["right_reaches"]]
  # Check
  stopifnot(is.numeric(left_reach) & is.numeric(right_reach))
  
  #Define maximum values for each reach
  data_length = length(in_counts)
  max_left_reach <- contender_chng_pt - 1
  max_right_reach <- data_length - contender_chng_pt + 1
  
  stopifnot(left_reach <= max_left_reach)
  stopifnot(right_reach <= max_right_reach)
  
  left_fence  = contender_chng_pt - left_reach
  right_fence = contender_chng_pt + right_reach - 1
  

  
  # Define each "clique" to be the grouped observations on either side of the changpoint
  left_clique  = in_counts[left_fence : (contender_chng_pt-1)]
  right_clique = in_counts[contender_chng_pt : right_fence]
  
  # Define the "clan" as the datapoints encompassed by the contender changepoint's reach
  clan = c(left_clique,right_clique)

  left_sum  = sum(left_clique)
  right_sum = sum(right_clique)
  clan_sum  = sum(left_sum,right_sum)
  clan_diff = right_sum - left_sum

  left_avg  = left_sum  / left_reach
  right_avg = right_sum / right_reach
  clan_avg  = clan_sum/(left_reach+right_reach)
  
  # Calculate the likelihood ratio for the given parameters assuming a difference between the cliques
  # and no-difference between the cliques
  options(warn=-1)
  like =  - clan_avg + right_avg + left_avg  -
            (clan_diff / 2) * (log(right_avg) - log(left_avg)) +
            log(besselI(nu = clan_diff, x = 2 * clan_avg )) -
            log(besselI(nu = clan_diff, x = 2*sqrt(left_avg*right_avg)))
  options(warn=0)

  # If the LR is (=/-)Inf, replace the value
  if(left_avg == right_avg |  like == Inf | like == -Inf | is.na(like) | is.nan(like) ){
    like = NA
  }
  
  return(like)
}

