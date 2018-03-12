#' @title Skellam Likelihood ratio for varying window sizes
#'
#' @description Takes in a data frame of candidate changepoints and clique 
#'     capacities, then calculates the likelihood ratio for the difference
#'     of each clique.
#' 
#' @param in_counts A dataframe containing the observed counts
#' @param contender_and_reaches A dataframe containing the candidate 
#'     changepoints along with the maximum group sizes separated by their 
#'     respective changepoint.
#' 
#' @return 
#' @export


skellamLR = function(in_counts,contender_and_reaches){
  # Check that `contender_and_reaches` is a dataframe
  if(!is.data.frame(contender_and_reaches)){
    stop("contender_and_reaches must be a dataframe with three columns")
  }

  cp_contender  <- contender_and_reaches[["cp_contender"]]
  
  left_reach <- contender_and_reaches[["left_reach"]]
  right_reach <- contender_and_reaches[["right_reach"]]
  
  #Define maximum values for each reach
  data_length = length(in_counts)
  max_left_reach <- cp_contender - 1
  max_right_reach <- data_length - cp_contender + 1
  
  stopifnot(left_reach <= max_left_reach)
  stopifnot(right_reach <= max_right_reach)
  
  left_fence  = cp_contender - left_reach
  right_fence = cp_contender + right_reach - 1
  

  
  # Define each "clique" to be the grouped observations on either side of the changpoint
  left_clique  = in_counts[left_fence : (cp_contender-1)]
  right_clique = in_counts[cp_contender : right_fence]
  
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

