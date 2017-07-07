BANOVA.floodlight <-
  function(sol, var_numeric, var_factor){
    if(class(sol) %in% c('BANOVA', 'BANOVA.Normal', 'BANOVA.T', 'BANOVA.Poisson', 'BANOVA.Bern', 'BANOVA.Bin', 'BANOVA.ordMultinomial', 'BANOVA.Multinomial')){
      
      if (sol$single_level){
        if (sol$model_name == 'BANOVA.Multinomial'){
          sol_tables <- floodlight.analysis(var_numeric, var_factor, sol$samples_l1_param, sol$dMatrice$X_full[[1]], sol$dMatrice$Z, dataX= sol$dataX, dataZ = sol$dataZ)
        }else{
          sol_tables <- floodlight.analysis(var_numeric, var_factor, sol$samples_l1_param, sol$dMatrice$X, sol$dMatrice$Z, data = sol$data)
        }
      }else{
        if (sol$model_name == 'BANOVA.Multinomial'){
          sol_tables <- floodlight.analysis(var_numeric, var_factor, sol$samples_l2_param, sol$dMatrice$X_full[[1]], sol$dMatrice$Z, dataX = sol$dataX, dataZ = sol$dataZ)
        }else{
          sol_tables <- floodlight.analysis(var_numeric, var_factor, sol$samples_l2_param, sol$dMatrice$X, sol$dMatrice$Z, data = sol$data)
        }
      }
      class(sol_tables) <- 'BANOVA.floodlight'
      return(sol_tables)
    }else{
      stop('Model is not recognized')
    }
  }