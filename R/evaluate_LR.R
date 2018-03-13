#' @title evaluate the log of the skellam likelihood ratio
#' 
#' @description A function to directly calcualte the likelihood ratio for the skellam distribution
#' 
#' 
#' 
#' @export
#' 


evaluate_LR <- function (clan_avg,right_avg,left_avg,clan_diff) {
  like = - clan_avg + right_avg + left_avg  -
    (clan_diff / 2) * (log(right_avg) - log(left_avg)) +
    log(besselI(nu = clan_diff, x = 2 * clan_avg )) -
    log(besselI(nu = clan_diff, x = 2*sqrt(left_avg*right_avg)))
}