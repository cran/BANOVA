BANOVA.floodlight <-
  function(sol, var_numeric, var_factor){
    if(class(sol) %in% c('BANOVA.Normal', 'BANOVA.T', 'BANOVA.Poisson', 'BANOVA.Bern', 'BANOVA.Bin', 'BANOVA.ordMultinomial', 'BANOVA.Multinomial')){
      
      sol_tables <- floodlight.analysis(var_numeric, var_factor, sol$samples_l2_param, sol$dMatrice$X, sol$dMatrice$Z, sol$data)
      class(sol_tables) <- 'BANOVA.floodlight'
      return(sol_tables)
    }else{
      stop('Model is not recognized')
    }
  }