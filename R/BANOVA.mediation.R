###
# This version, the mediation analysis only includes one mediator
###
BANOVA.mediation <-
  function(sol_1, sol_2, xvar, mediator){
    if(!(class(sol_1) %in% c('BANOVA', 'BANOVA.Normal', 'BANOVA.T', 'BANOVA.Poisson', 'BANOVA.Bern', 'BANOVA.Bin', 'BANOVA.ordMultinomial', 'BANOVA.Multinomial'))) stop('Model is not recognized')
    if(sol_2$model_name != 'BANOVA.Normal') stop('The mediator must follow the Normal distribution, use BANOVA Normal models instead.')
    
    X_names = colnames(sol_1$dMatrice$X)
    Z_names = colnames(sol_1$dMatrice$Z)
    X_assign = attr(sol_1$dMatrice$X, 'assign')
    Z_assign = attr(sol_1$dMatrice$Z, 'assign')
    num_l1 <- length(X_assign)
    num_l2 <- length(Z_assign)
    if (sol_1$single_level)
      samples_l2_param <- sol_1$samples_l1_param
    else
      samples_l2_param <- sol_1$samples_l2_param
    n_sample <- nrow(samples_l2_param)
    est_matrix <- array(0 , dim = c(num_l1, num_l2, n_sample), dimnames = list(X_names, Z_names, NULL))
    for (i in 1:num_l1){
      for (j in 1:n_sample)
        est_matrix[i,,j] <- samples_l2_param[j,((i-1)*num_l2+1):((i-1)*num_l2+num_l2)]
    }
    
    # mediator
    X_names_m = colnames(sol_2$dMatrice$X)
    Z_names_m = colnames(sol_2$dMatrice$Z)
    X_assign_m = attr(sol_2$dMatrice$X, 'assign')
    Z_assign_m = attr(sol_2$dMatrice$Z, 'assign')
    num_l1_m <- length(X_assign_m)
    num_l2_m <- length(Z_assign_m)
    if (sol_2$single_level)
      samples_l2_param_m <- sol_2$samples_l1_param
    else
      samples_l2_param_m <- sol_2$samples_l2_param
    n_sample_m <- nrow(samples_l2_param_m)
    est_matrix_m <- array(0 , dim = c(num_l1_m, num_l2_m, n_sample_m), dimnames = list(X_names_m, Z_names_m, NULL))
    for (i in 1:num_l1_m){
      for (j in 1:n_sample_m)
        est_matrix_m[i,,j] <- samples_l2_param_m[j,((i-1)*num_l2_m+1):((i-1)*num_l2_m+num_l2_m)]
    }

    #################
    if(0){ # temporarily replaced by the function cal.mediation.effects
    model1_level1_var_matrix <- attr(attr(sol_1$mf1, 'terms'),'factors')
    model1_level1_var_dataClasses <- attr(attr(sol_1$mf1, 'terms'),'dataClasses')
    model1_level2_var_matrix <- attr(attr(sol_1$mf2, 'terms'),'factors')
    model1_level2_var_dataClasses <- attr(attr(sol_1$mf2, 'terms'),'dataClasses')
    # find the direct effects
    # find corresponding names in X or Z for xvar, see floodlight analysis, then used in est_matrix
    xvar_in_l1 <- xvar %in% rownames(model1_level1_var_matrix)
    xvar_in_l2 <- xvar %in% rownames(model1_level2_var_matrix)
    if (!xvar_in_l1 & !xvar_in_l2) stop("xvar is not included in the model!")
    if (xvar_in_l1){
      attr(xvar, 'class') = model1_level1_var_dataClasses[xvar]
      xvar_index <- which(rownames(model1_level1_var_matrix) == xvar)
      xvar_index_assign <- which(model1_level1_var_matrix[xvar_index, ] == 1)
      xvar_related_names <- X_names[which(X_assign %in% xvar_index_assign)]
      for (xvar_name in xvar_related_names){
        print('Direct effects:')
        est_samples <- array(0, dim = c(n_sample))
        for (n_s in 1:n_sample){
          est_samples[n_s] <- est_matrix[xvar_name, 1, n_s]
        }
        direct_effec_mean <- mean(est_samples)
        quantiles <- quantile(est_samples, c(0.025, 0.975))
        tmp_output <- array(0, dim = c(1,3), dimnames = list(NULL, c('mean', '2.5%', '97.5%')))
        tmp_output[1,1] <- direct_effec_mean
        tmp_output[1,2:3] <- quantiles
        print(tmp_output)
      }
    }else{
      attr(xvar, 'class') = model1_level2_var_dataClasses[xvar]
      xvar_index <- which(rownames(model1_level2_var_matrix) == xvar)
      xvar_index_assign <- which(model1_level2_var_matrix[xvar_index, ] == 1)
      xvar_related_names <- Z_names[which(Z_assign %in% xvar_index_assign)]
      for (xvar_name in xvar_related_names){
        print('Direct effects:')
        est_samples <- array(0, dim = c(n_sample))
        for (n_s in 1:n_sample){
          est_samples[n_s] <- est_matrix[1, xvar_name, n_s]
        }
        direct_effec_mean <- mean(est_samples)
        quantiles <- quantile(est_samples, c(0.025, 0.975))
        tmp_output <- array(0, dim = c(1,3), dimnames = list(NULL, c('mean', '2.5%', '97.5%')))
        tmp_output[1,1] <- direct_effec_mean
        tmp_output[1,2:3] <- quantiles
        print(tmp_output)
      }
    }
    }
    ##################
    sol <- list()
    # calculate direct effect of xvar in model 1
    direct_effects <- cal.mediation.effects(sol_1, est_matrix, n_sample, xvar, mediator)
    sol$dir_effects <- list()
    for (i in 1:length(direct_effects)){
      # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
      idx_to_rm <- c()
      for (j in 1:ncol(direct_effects[[i]]$table_m)){
        if (all(direct_effects[[i]]$table_m[, j] == '1') || all(direct_effects[[i]]$table_m[, j] == 1))
          idx_to_rm <- c(idx_to_rm, j)
      }
      if(length(idx_to_rm) > 0)
        sol$dir_effects[[i]] <- direct_effects[[i]]$table_m[, -idx_to_rm]
      else
        sol$dir_effects[[i]] <- direct_effects[[i]]$table_m

    }
    
    # calculate effects of the mediator in model 1
    mediator_l1_effects <- cal.mediation.effects(sol_1, est_matrix, n_sample, mediator, xvar)
    sol$m1_effects <- list()
    for (i in 1:length(mediator_l1_effects)){
      # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
      idx_to_rm <- c()
      for (j in 1:ncol(mediator_l1_effects[[i]]$table_m)){
        if (all(mediator_l1_effects[[i]]$table_m[, j] == '1') || all(mediator_l1_effects[[i]]$table_m[, j] == 1))
          idx_to_rm <- c(idx_to_rm, j)
      }
      if(length(idx_to_rm) > 0)
        sol$m1_effects[[i]] <- mediator_l1_effects[[i]]$table_m[, -idx_to_rm]
      else
        sol$m1_effects[[i]] <- mediator_l1_effects[[i]]$table_m
      
    }
    
    # calculate effects of the xvar in model 2
    mediator_xvar_effects <- cal.mediation.effects(sol_2, est_matrix_m, n_sample_m, xvar)
    sol$m2_effects <- list()
    for (i in 1:length(mediator_xvar_effects)){
      mediator_xvar_effects[[i]]$table_m <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$table_m), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$table_m)
      mediator_xvar_effects[[i]]$index_name <- cbind(array(1, dim = c(nrow(mediator_xvar_effects[[i]]$index_name), 1), dimnames = list(NULL, mediator)), mediator_xvar_effects[[i]]$index_name)
      # filter columns with only "1" or 1 (numeric), TODO: here 1 is hard coded, think about a better way to determine if it is numeric
      idx_to_rm <- c()
      for (j in 1:ncol(mediator_xvar_effects[[i]]$table_m)){
        if (all(mediator_xvar_effects[[i]]$table_m[, j] == '1') || all(mediator_xvar_effects[[i]]$table_m[, j] == 1))
          idx_to_rm <- c(idx_to_rm, j)
      }
      if(length(idx_to_rm) > 0)
        sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m[, -idx_to_rm]
      else
        sol$m2_effects[[i]] <- mediator_xvar_effects[[i]]$table_m

    }

    k <- 1
    sol$indir_effects <- list()
    for (i in 1:length(mediator_l1_effects))
      for (j in 1:length(mediator_xvar_effects)){
        indirect_effects <- combine.effects (mediator_l1_effects[[i]], mediator_xvar_effects[[j]])
        idx_to_rm <- c()
        for (j in 1:ncol(indirect_effects)){
          if (all(indirect_effects[, j] == '1') || all(indirect_effects[, j] == 1))
            idx_to_rm <- c(idx_to_rm, j)
        }
        if(length(idx_to_rm) > 0)
            sol$indir_effects[[k]] <- indirect_effects[, -idx_to_rm]
        else
            sol$indir_effects[[k]] <- indirect_effects
        k <- k + 1
      }
    sol$xvar = xvar
    sol$mediator = mediator
    class(sol) <- 'BANOVA.mediation'
    return(sol)
  }

combine.effects <- function (mediator_l1_effects, mediator_xvar_effects){
  table_1_names <- mediator_l1_effects$index_name
  table_2_names <- mediator_xvar_effects$index_name
  # find common columns 
  temp_1 <- mediator_l1_effects$index
  colnames(temp_1) <- paste(colnames(temp_1), '.1', sep = "")
  table_1_names_index <- cbind(table_1_names, temp_1)
  temp_2 <- mediator_xvar_effects$index
  colnames(temp_2) <- paste(colnames(temp_2), '.2', sep = "")
  table_2_names_index <- cbind(table_2_names, temp_2)
  table_2_names_index.df <- table_2_names_index
  table_1_names_index.df <- table_1_names_index
  temp_table_index <- merge(table_2_names_index.df, table_1_names_index.df, by = intersect(colnames(table_1_names), colnames(table_2_names)), all.x = T)
  table_1_est_sample_index <- temp_table_index[,colnames(temp_1)]
  table_2_est_sample_index <- temp_table_index[,colnames(temp_2)]
  result_table <- array('1', dim = c(nrow(temp_table_index), ncol(temp_table_index) - 4 + 3), dimnames = list(rep("",nrow(temp_table_index)), c(union(colnames(table_1_names), colnames(table_2_names)), 'mean', '2.5%', '97.5%')))
  for (nm in union(colnames(table_1_names), colnames(table_2_names)))
  result_table[, nm] <- as.character(temp_table_index[[nm]])
  for (ind in 1:nrow(table_1_est_sample_index)){
    common_n_sample <- min(dim(mediator_l1_effects$samples)[3], dim(mediator_xvar_effects$samples)[3])
    m_samples <- mediator_l1_effects$samples[table_1_est_sample_index[ind,1], table_1_est_sample_index[ind,2], 1:common_n_sample] * mediator_xvar_effects$samples[table_2_est_sample_index[ind,1], table_2_est_sample_index[ind,2], 1:common_n_sample]
    result_table[ind,'mean'] <- round(mean(m_samples), 4)
    result_table[ind,c('2.5%', '97.5%')] <- round(quantile(m_samples, probs = c(0.025, 0.975)),4)
  }
  return(result_table)
}