# find design matrix of coefficients of a mediator (including interactions which contains the mediator, using summation)
# the mediator must be included in each interaction
effect.matrix.mediator <- 
function (interaction_factors=list(), mediator=NA, matrix_formula=NULL, xvar=NA){
  # generate the effect matrix for each interaction, numerical covariates excluded, TODO: may have various levels for numeric variables
  # Args:
  #     interaction_factors       : a list of values of factors in the interaction to generate the effect matrix
  #     assign        : Not used for the new method, to remove
  #     index_factor  : Not used, to remove
  #     numeric_index : Not used, to remove
  # Returns:
  #     an effect matrix
  #
  if (length(interaction_factors) > 0){
    num_inter = length(interaction_factors)
    levels_inter = list()
    names_inter = list()
    for (i in 1:num_inter){
      names_inter[[i]] = attr(interaction_factors[[i]], 'var_names')
      if (is.factor(interaction_factors[[i]])){ 
        levels_inter[[attr(interaction_factors[[i]], 'var_names')]] = levels(interaction_factors[[i]])
      }else{
        # for numeric or integer var, use 1 instead, e.g. the mediator
        levels_inter[[attr(interaction_factors[[i]], 'var_names')]] = 1
      }
    }
    factors_inter = expand.grid(levels_inter)
    temp_fun <- function(x){
      if (length(unique(x)) > 1)
        as.factor(x)
      else
        x
    }
    factors_inter_factor = as.data.frame(lapply(factors_inter, temp_fun))
    formula_inter = paste(names_inter, collapse = '*')
    if (is.null(matrix_formula))
      eval(parse(text = paste('model_frame <- model.frame(~',formula_inter,', data = factors_inter_factor)', sep='')))
    else
      model_frame <- model.frame(matrix_formula, data = factors_inter_factor)
      #eval(parse(text = paste('model_frame <- model.frame(',matrix_formula,', data = factors_inter_factor)', sep='')))

    effect_matrix <- model.matrix(formula(attr(model_frame, 'terms')), data = factors_inter_factor)
    attr(effect_matrix, 'levels') = as.matrix(factors_inter)
    if (!is.na(mediator)){
      # only calculate the summation of coefficients that related to the mediator exclude xvar
      if (!is.na(xvar) & (xvar %in% rownames(attr(attr(model_frame, 'terms'), 'factors')))){
        tmp_mtx <- attr(attr(model_frame, 'terms'), 'factors')
        tmp_ind_m <- which(rownames(tmp_mtx) == mediator)
        tmp_ind_x <- which(rownames(tmp_mtx) == xvar)
        assign_selected <- which(tmp_mtx[tmp_ind_m, ] == 1 & tmp_mtx[tmp_ind_x, ] == 0) 
        #assign_selected <- which(attr(attr(model_frame, 'terms'), 'factors')[mediator, ] == 1 & attr(attr(model_frame, 'terms'), 'factors')[xvar, ] == 0) 
      }else{
        tmp_mtx <- attr(attr(model_frame, 'terms'), 'factors')
        tmp_ind <- which(rownames(tmp_mtx) == mediator)
        assign_selected <- which(tmp_mtx[tmp_ind, ] == 1)
        #assign_selected <- which(attr(attr(model_frame, 'terms'), 'factors')[mediator, ] == 1)
      }
      column_selected <- which(attr(effect_matrix, 'assign') %in% assign_selected)
      effect_matrix_selected <- effect_matrix[ , column_selected, drop=FALSE]
    }else{
      if (!is.na(xvar) & (xvar %in% rownames(attr(attr(model_frame, 'terms'), 'factors')))){
        # exclude xvar
        tmp_mtx <- attr(attr(model_frame, 'terms'), 'factors')
        tmp_ind_x <- which(rownames(tmp_mtx) == xvar)
        assign_selected <- which(tmp_mtx[tmp_ind_x, ] == 0)
        column_selected <- which(attr(effect_matrix, 'assign') %in% assign_selected)
        effect_matrix_selected <- effect_matrix[ , column_selected, drop=FALSE]
      }else{
        effect_matrix_selected <- effect_matrix
      }
    }
    
    attr(effect_matrix_selected, 'levels') = as.matrix(factors_inter)
  }else{
    effect_matrix_selected = NA
  }
  return(effect_matrix_selected)
}
