print.BANOVA.floodlight <- 
  function(x, ...){
    within_range <- array(0, dim = c(nrow(x$sol), ncol(x$sol)))
    for (i in 1:nrow(x$sol)){
      for (j in 1:ncol(x$sol)){
        if (x$sol[i, j] > x$num_range[1] & x$sol[i, j] < x$num_range[2])
          within_range[i,j] <- 1
      }
    }
    x$sol <- format(round(x$sol, digits = 4), trim = T, nsmall = 4)
    
    for (i in 1:nrow(x$sol)){
      for (j in 1:ncol(x$sol)){
        if (within_range[i,j])
          x$sol[i, j] <- paste(x$sol[i, j], '*', sep="")
      }
    }
    print(noquote(x$sol))
  }