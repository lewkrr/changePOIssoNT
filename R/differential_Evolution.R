#' @title Differential evolution for discrete data with finite support.
#' @description FIXME
#' 
#' @include window_grid.R
#' @include skellamLR.R
#' 
#' @importFrom truncnorm rtruncnorm

differential.Evolution <- function( in_counts , contenter_chng_pts ,   n_gen = 1e6){
  
  # Initialize generation
  G = 0
  # From input candidate-change-points, calculate the MAX window sizes on each side
  data_length = N = length(in_counts)
  Nchange <- length(contenter_chng_pts)
  
  max_left_reach  = contenter_chng_pts - as.integer(1)
    left_low  = floor(max_left_reach/6)
    left_high = floor(max_left_reach*5/6)
    left_out = map_dbl( .x = left_high - left_low , .f = min, 20  )
    left_gridpoints = pmap( .l = list( from = left_low, to = left_high, length.out = left_out ) , .f = seq)
  max_right_reach = data_length - contenter_chng_pts + as.integer(1)
    right_low  = floor(max_right_reach/6)
    right_high = floor(max_right_reach*5/6)
    right_out = map_dbl( .x = right_high - right_low , .f = min, 20  )
    right_gridpoints = pmap( .l = list( from = right_low, to = right_high, length.out = right_out ) , .f = seq)
    
    # Merge the `left_gridpoints` and `right_gridpoints` lists so that each set is paired
    # for use in the cross_df function
    paired_grid_values <- map2(.x = left_gridpoints , .y = right_gridpoints, list )
    temp_grid_values <- paired_grid_values
    
    rename_grid_values <- function(grid){
      names(grid) <- c("left_reaches","right_reaches")
      return(grid)
    }
    
    grid_of_reaches <- 
      temp_grid_values %>% 
      map(rename_grid_values) %>% 
      map(cross_df) %>% 
      map(mutate, obj_func = numeric(1) )
    
  
  contender_and_reaches <- 
    tibble(
      contenter_chng_pt = contenter_chng_pts,
      max_left_reach  = contenter_chng_pts - as.integer(1),
      max_right_reach = data_length - contenter_chng_pts + as.integer(1),
      grid_of_reaches = grid_of_reaches,
      best_left_reach  = integer(1),
      best_right_reach = integer(1),
      objectiv_function = numeric(1)
    )
  
  # FIXME
  # Evaluate the initial grids, then sort by rank
  contender_and_reaches["grid_of_reaches"] %>% map(skellamLR)
  
  
  
  
  #FIXME
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
  while( G < n_gen ){
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
      target.and.donor = lapply( best.and.randPair , mutate.eqtn.7 , in_counts )

      # Crossover/Mate according to equation 5 #
      U.i = lapply(target.and.donor , cross.over.eqtn.5 , in_counts)

      # Select according to equation 6
      X.i.plus.1 = lapply(U.i , selection.eqtn.6 , in_counts)

      # Convert generation 2 to a data.frame
      pop.i.G.plus.1.df = do.call(rbind.data.frame , X.i.plus.1)

      # Remove duplicates
      #dupes = duplicated(pop.i.G.plus.1.df)
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

        new.obs$ObjFunc = skellamLR(new.obs,in_counts)

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
      lapply(best.list , integerNM , in_counts)
    }
    G = G + 1
  }
}

windows = data.frame( chng.pt = c(92,152,395) , MAX.lft = c(91,151,358) , MAX.rt=c(421,361,154) )

windows = windows[3,]

#windows = full.windows
