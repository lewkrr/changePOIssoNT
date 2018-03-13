
#' @importFrom data.table setkey

window.grid <- function(windows){

  chng.pt = as.numeric(windows[1]) # chng.pt
  MAX.lft = as.numeric(windows[2]) # MAX.lft
  MAX.rt  = as.numeric(windows[3]) # MAX.rt

  # Define the size of the grid steps
  left.step.size  = min( ceiling(MAX.lft/6) , 100000 )
  right.step.size = min( ceiling(MAX.rt/6) , 100000 )

  #build grid margins
  left.seq  = seq( left.step.size  , MAX.lft , left.step.size )
  right.seq = seq( right.step.size , MAX.rt  , right.step.size )

  # Create unique points from grid margins
  grid.points =  expand.grid(left.seq,right.seq)
  n.pts = nrow(grid.points)

  popltn.grid =  data.frame( chng.pt    = rep(chng.pt,n.pts) ,
                             left.size  = grid.points$Var1   ,
                             right.size = grid.points$Var2   ,
                             MAX.lft    = rep(MAX.lft,n.pts) ,
                             MAX.rt     = rep(MAX.rt,n.pts)   )

  # Evaluate the objective function of each point
  popltn.grid$ObjFunc = apply( popltn.grid , MARGIN = 1, skellamLR , in.data = in.data)

  # FIXME test data.table sort functions
  loc = which(popltn.grid$ObjFunc == min(popltn.grid$ObjFunc , na.rm = TRUE))[1]




  return(popltn.grid)
}
