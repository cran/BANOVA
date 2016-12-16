table.predictions <-
function(x){
  if(class(x) == 'BANOVA.Normal' || class(x) == 'BANOVA.T'){
    sol_tables <- print.table.means(x$coef.tables$coeff_table, x$samples_l2_param, colnames(x$dMatrice$X), X_assign = attr(x$dMatrice$X, 'assign'), 
                      X_classes = attr(x$dMatrice$X, 'dataClasses'), colnames(x$dMatrice$Z), Z_assign = attr(x$dMatrice$Z, 'assign'), 
                      Z_classes = attr(x$dMatrice$Z, 'dataClasses'), l1_values = attr(x$dMatrice$X, 'varValues'), 
                      l1_interactions = attr(x$dMatrice$X, 'interactions'), l1_interactions_index = attr(x$dMatrice$X, 'interactions_index'), 
                      l2_values = attr(x$dMatrice$Z, 'varValues'), l2_interactions = attr(x$dMatrice$Z, 'interactions'), 
                      l2_interactions_index = attr(x$dMatrice$Z, 'interactions_index'), numeric_index_in_X = attr(x$dMatrice$X, 'numeric_index'),
                      numeric_index_in_Z = attr(x$dMatrice$Z, 'numeric_index'), model = 'NormalNormal')
  }else if (class(x) == 'BANOVA.Poisson'){
    sol_tables <- print.table.means(x$coef.tables$coeff_table, x$samples_l2_param, colnames(x$dMatrice$X), X_assign = attr(x$dMatrice$X, 'assign'), 
                      X_classes = attr(x$dMatrice$X, 'dataClasses'), colnames(x$dMatrice$Z), Z_assign = attr(x$dMatrice$Z, 'assign'), 
                      Z_classes = attr(x$dMatrice$Z, 'dataClasses'), l1_values = attr(x$dMatrice$X, 'varValues'), 
                      l1_interactions = attr(x$dMatrice$X, 'interactions'), l1_interactions_index = attr(x$dMatrice$X, 'interactions_index'), 
                      l2_values = attr(x$dMatrice$Z, 'varValues'), l2_interactions = attr(x$dMatrice$Z, 'interactions'), 
                      l2_interactions_index = attr(x$dMatrice$Z, 'interactions_index'), numeric_index_in_X = attr(x$dMatrice$X, 'numeric_index'),
                      numeric_index_in_Z = attr(x$dMatrice$Z, 'numeric_index'), model = 'PoissonNormal', l2_sd = x$samples_l2_sigma_param)
  }else if (class(x) == 'BANOVA.Bern' || class(x) == 'BANOVA.Bin' ){
    sol_tables <- print.table.means(x$coef.tables$coeff_table, x$samples_l2_param, colnames(x$dMatrice$X), X_assign = attr(x$dMatrice$X, 'assign'), 
                      X_classes = attr(x$dMatrice$X, 'dataClasses'), colnames(x$dMatrice$Z), Z_assign = attr(x$dMatrice$Z, 'assign'), 
                      Z_classes = attr(x$dMatrice$Z, 'dataClasses'), l1_values = attr(x$dMatrice$X, 'varValues'), 
                      l1_interactions = attr(x$dMatrice$X, 'interactions'), l1_interactions_index = attr(x$dMatrice$X, 'interactions_index'), 
                      l2_values = attr(x$dMatrice$Z, 'varValues'), l2_interactions = attr(x$dMatrice$Z, 'interactions'), 
                      l2_interactions_index = attr(x$dMatrice$Z, 'interactions_index'), numeric_index_in_X = attr(x$dMatrice$X, 'numeric_index'),
                      numeric_index_in_Z = attr(x$dMatrice$Z, 'numeric_index'), model = 'BernNormal')
  }else if (class(x) == 'BANOVA.ordMultinomial'){
    sol_tables <- print.table.means(x$coef.tables$coeff_table, x$samples_l2_param, colnames(x$dMatrice$X), X_assign = attr(x$dMatrice$X, 'assign'), 
                      X_classes = attr(x$dMatrice$X, 'dataClasses'), colnames(x$dMatrice$Z), Z_assign = attr(x$dMatrice$Z, 'assign'), 
                      Z_classes = attr(x$dMatrice$Z, 'dataClasses'), l1_values = attr(x$dMatrice$X, 'varValues'), 
                      l1_interactions = attr(x$dMatrice$X, 'interactions'), l1_interactions_index = attr(x$dMatrice$X, 'interactions_index'), 
                      l2_values = attr(x$dMatrice$Z, 'varValues'), l2_interactions = attr(x$dMatrice$Z, 'interactions'), 
                      l2_interactions_index = attr(x$dMatrice$Z, 'interactions_index'), numeric_index_in_X = attr(x$dMatrice$X, 'numeric_index'),
                      numeric_index_in_Z = attr(x$dMatrice$Z, 'numeric_index'), samples_cutp_param = x$samples_cutp_param, model = 'MultinomialordNormal')
  }else if (class(x) == 'BANOVA.Multinomial'){
    sol_tables <- multi.print.table.means (x$coef.tables$coeff_table, n_choice = x$n_categories, x$samples_l2_param, colnames(x$dMatrice$X_full[[1]]), X_assign = attr(x$dMatrice$X_full[[1]], 'assign'), 
                             X_classes = attr(x$dMatrice$X_full[[1]], 'dataClasses'), colnames(x$dMatrice$Z), Z_assign = attr(x$dMatrice$Z, 'assign'), 
                             Z_classes = attr(x$dMatrice$Z, 'dataClasses'), l1_values = attr(x$dMatrice$X_full[[1]], 'varValues'), 
                             l1_interactions = attr(x$dMatrice$X_full[[1]], 'interactions'), l1_interactions_index = attr(x$dMatrice$X_full[[1]], 'interactions_index'), 
                             l2_values = attr(x$dMatrice$Z, 'varValues'), l2_interactions = attr(x$dMatrice$Z, 'interactions'), 
                             l2_interactions_index = attr(x$dMatrice$Z, 'interactions_index'), numeric_index_in_X = attr(x$dMatrice$X_full[[1]], 'numeric_index'),
                             numeric_index_in_Z = attr(x$dMatrice$Z, 'numeric_index'))
  }
  invisible(sol_tables)
}