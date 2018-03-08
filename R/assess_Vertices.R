#' @title Set up the data for the NM algorithm
#'
#' @description Takes the input values, calcs StdDevs and means, then
#' @include skellamLR.R
#' @export

assess.Vertices <- function(vertex.list , in.data){
  if(!is.data.frame(vertex.list)){ stop("the vertex argumnt 'rand.vertices' must be of type data.frame") }

  rand.vertices = vertex.list$vertices

  rand.vertices$ObjFunc = apply( X = rand.vertices , MARGIN = 1 , skellamLR , in.data )
}
