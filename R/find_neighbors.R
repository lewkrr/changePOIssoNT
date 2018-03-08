#' 
#' @export

find.neighbors <- function( target , nbrhd.sz , in.df  ){
  temp.df = in.df

  # Calculate distances from target
  temp.df$dist = sqrt((in.df[,2] - target[,2])^2 + (in.df[,3] - target[,3])^2)

  if( sum( !is.na( temp.df$ObjFunc ) ) >= 4){
    rows.to.keep = !is.na( temp.df$ObjFunc ) & temp.df$dist != 0
    temp.df = temp.df[rows.to.keep,]
  }

  #temp.df = temp.df[order(temp.df$dist,-temp.df$ObjFunc),]

  # Find obs within neighborhood size
  rows.to.keep = (temp.df$dist <= nbrhd.sz) & (temp.df$dist > 0)
  obs.to.keep = temp.df[rows.to.keep,]

  if(nrow(obs.to.keep) < 3){
    temp.df = temp.df[order(temp.df$dist,-temp.df$ObjFunc),]
    itr = 1
    while( nrow(obs.to.keep) < 3 & itr < nrow(in.df) ){
      num.obs.kept = nrow(obs.to.keep)
      obs.to.keep = temp.df[1:(num.obs.kept+1),]
      itr = itr + 1
    }
  }

  point.and.nbrs = list(target , obs.to.keep)
  names(point.and.nbrs) = c("target","neighbors")

  return(point.and.nbrs)
}
