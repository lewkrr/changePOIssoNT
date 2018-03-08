
#' @importFrom data.table duplicated
#' @importFrom truncnorm rtruncnorm

differential.Evolution <- function( in.data , candidate.chng.pts ,   Gmax = 1e6){
  # Initialize generation
  G = 0
  # From input candidate-change-points, calculate the MAX window sizes on each side
  data.dim = N = length(in.data)

  windows = data.frame( chng.pt = candidate.chng.pts ,
                        MAX.lft = candidate.chng.pts - 1 ,
                        MAX.rt  = N - candidate.chng.pts + 1 )

  # Create grid as initial population
  #NP = population size
  # Evaluate the initial grid, sort by rank
  population.dfs.list = apply( windows , MARGIN = 1 , window.grid )

  # Store best inidividual coordinate thus far for first arrangement
  best.list = lapply( population.dfs.list , FUN = find.best.of.df )

  #Ncp = Number of candidate change-points
  Ncp = length(best.list)


  # D = initial neighborhood size
  D = D.list = lapply(best.list , FUN = function(in.df) ceiling(sqrt( 4*min( in.df$MAX.lft/6 , 100000 )^2 + 4*min( in.df$MAX.rt/6 , 100000 )^2 ))  )

  # Define decay rate function so that neighborhood size ends at 7
  Decay <- function( G , D , Gmax , final.window = 3.5 ){
    Kappa = ( log(D) - log(final.window) )/Gmax
    nbr.sz = D*exp( -Kappa * G )
    return( nbr.sz )
  }

  # REPEAT
  while( G < Gmax ){
    for( i in 1:Ncp ){
      D.i = Decay( G , D = D[[i]] , Gmax  )
      pop.i.G.df = population.dfs.list[[i]]
      pop.i.G.list = split( pop.i.G.df , seq(nrow(pop.i.G.df)))


      # Identify K nearest neighbors for each candidate in pop.i.G.df
      #neighborhoods = find.k.neighbors(pop.i.G.df, k=5)

      # Identify neighbors within a given distance, D
      #distances.coord = as.matrix(dist( pop.i.G.df[,c(2,3)] )) < D.i
      #diag(distances.coord) = FALSE

      #coord.list = split( distances.coord , seq(nrow(distances.coord)))


      dist.neighborhoods = lapply( pop.i.G.list , FUN = find.neighbors , nbrhd.sz = D.i , in.df = pop.i.G.df )

      # Identify BESTi (the neighbor with the lowest objective function)
      # Randomly sample two other neighbors
      best.and.randPair = lapply(dist.neighborhoods , FUN = love.thy.neighbors )

      # Mutate according to equation 7
      target.and.donor = lapply( best.and.randPair , mutate.eqtn.7 , in.data )

      # Crossover/Mate according to equation 5 #
      U.i = lapply(target.and.donor , cross.over.eqtn.5 , in.data)

      # Select according to equation 6
      X.i.plus.1 = lapply(U.i , selection.eqtn.6 , in.data)

      # Convert generation 2 to a data.frame
      pop.i.G.plus.1.df = do.call(rbind.data.frame , X.i.plus.1)

      # Remove duplicates
      dupes = duplicated(pop.i.G.plus.1.df)
      pop.i.G.plus.1.df = pop.i.G.plus.1.df[!dupes,]

      # return population to original size
      NP = length(pop.i.G.list)
      NP.less.dupes = nrow(pop.i.G.plus.1.df)
      while(NP.less.dupes < NP){
        new.obs = pop.i.G.plus.1.df[sample(NP.less.dupes,1),]

        new.obs[,c(2,3)] = round(
                              rtruncnorm(n = 1 ,
                                         a = c(2,2) ,
                                         b = as.numeric(new.obs[,c(4,5)]) ,
                                         mean = as.numeric(new.obs[,c(2,3)]) ,
                                         sd = rep(sqrt(D.i),2)
                          ))

        new.obs$ObjFunc = skellamLR(new.obs,in.data)

        pop.i.G.plus.1.df = rbind(pop.i.G.plus.1.df,new.obs)

        NP.less.dupes = nrow(pop.i.G.plus.1.df)
      }

      # If nothing better than before, replace the worst of the offspring with the best thus far.

      # Identify the best offspring
      loc.min = which(pop.i.G.plus.1.df$ObjFunc == min(pop.i.G.plus.1.df$ObjFunc, na.rm=TRUE))

      # Identify which values have NA values in the ObjFunc
      whichNA = which(is.na(pop.i.G.plus.1.df$ObjFunc))

      # Verify that a best offspring does in fact exist.  If so store it
      if(length(loc.min) != 0) best.of.offspring = pop.i.G.plus.1.df[loc.min[1],]

      # If all offspring are NA, replace one offspring with best-thus-far
      if(length(loc.min) == 0 ){
        pop.i.G.plus.1.df[sample(which(is.na(pop.i.G.plus.1.df$ObjFunc)),1),] = best.list[[i]]

      }else if( length(whichNA) > 0 & (best.of.offspring$ObjFunc > best.list[[i]]$ObjFunc) ){
        #If the Best.of.Offspring is NOT best yet, replace an NA value with the best thus far.
        pop.i.G.plus.1.df[sample(which(is.na(pop.i.G.plus.1.df$ObjFunc)),1),] = best.list[[i]]

      }else if( best.of.offspring$ObjFunc < best.list[[i]]$ObjFunc ){
        #If new best is observed, update best.list
        best.list[[i]] = pop.i.G.plus.1.df[loc.min,]
      }

      population.dfs.list[[i]] = pop.i.G.plus.1.df

    }

    # Run the interger Nelder Mead algorithm on the best
    if( G %% 10 ){
      # Run integerNM of the best individual thus far for each population
      lapply(best.list , integerNM , in.data)
    }
    G = G + 1
  }
}

windows = data.frame( chng.pt = c(92,152,395) , MAX.lft = c(91,151,358) , MAX.rt=c(421,361,154) )

windows = windows[3,]

windows = full.windows
