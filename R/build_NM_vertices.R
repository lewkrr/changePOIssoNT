#' @title Set up the data for the NM algorithm
#'
#' @description Takes the input values, calcs StdDevs and means, then
#' @importFrom truncnorm rtruncnorm
#'

build.NM.vertices <- function(in.point,in.data){
  #if(!is.data.frame(in.point)){ stop("the data argumnt 'in.point' must be of type data.frame") }

  dim.lim = as.numeric(in.point[c(4,5)])
  v1 = v2 = v3 = in.point

  vertices = as.data.frame(rbind(v1,v2,v3))
  names(vertices) = c("chng.pt", "left.size", "right.size", "MAX.lft", "MAX.rt",   "ObjFunc")

  for(row in 1:3){
    vertices[row,c(2,3)] = round(rtruncnorm(1 , a = c(2,2) , b = dim.lim , mean = as.numeric(in.point[,c(2,3)]) ,  sd = dim.lim/64 ))
    vertices[row,]$ObjFunc = skellamLR(vertices[row,],in.data)
  }
  vertices = vertices[order(vertices$ObjFunc),]
  rownames(vertices) = c("best.v","middle.v","worst.v")

  centroid = in.point
  centroid[,c(2,3)] = round(( vertices["best",c(2,3)] + vertices["middle",c(2,3)] )/2)
  centroid$ObjFunc = skellamLR(centroid,in.data)

  point.and.vertices = list(in.point,vertices, centroid)
  names(point.and.vertices) = c("best","vertices","centroid")

  return(point.and.vertices)
}
