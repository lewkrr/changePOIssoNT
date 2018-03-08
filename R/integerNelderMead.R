#' @title Integer Nelder-Mead algorithm
#'
#' @description This function depends on other functions, both of which are
#'     lower in alphabetical order, and one of which in not exported.  This
#'     function's code was meant to illustrate how the @include tag allows
#'     one to load required code so that the present code is able to call
#'     upon the necessary functions.
#' # @include flattery.R unexported.R
#' @export
#' @importFrom spatstat nndist
#' @include build_NM_vertices.R


integerNM <- function( in.best.list , reflectionCoef = 1 , expandCoef = 0.5 , contractCoef = 0.75 , maxItr = 1000 , in.data){
  #if(!is.data.frame(in.best.list)){ stop("the data argumnt 'in.best.list' must be of type data.frame") }

  # generate vertices for each of the best points
  # Returns a list contianing the original (best) point,
  # a set of vertices ordered by Objective-Function,
  # and the centroid of the two worst
  vertex.list = lapply(in.best.list, build.NM.vertices , in.data)

  ### Run the actual NM algorithm on the vertices


  while( max.vertex.distance > sqrt(2.1) ){


    # REFLECT
    reflect.list = lapply(vertex.list , reflect , reflectionCoef , in.data )

    # EXPAND
    expanded.list = lapply(reflect.list , expand , expandCoef , in.data )

    # CONTRACT/SHRINK
    contracted = lapply(expanded.list , contract , in.data )
  }



  # return a data.frame the same as in.best.list
}
