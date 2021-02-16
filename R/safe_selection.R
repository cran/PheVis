#' safe_selection
#'
#' @description Select the variables from dataframe by removing the rare variables and apply 'SAFE' on it.
#'
#' @param df dataframe
#' @param var_surrogate variables used for building the surrogates
#' @param surrogate_quali surrogate with 3 values (0 and 1 the extremes and 3 middle patients)
#' @param threshold rareness threshold (default = 0.05).
#' @param alpha glmnet parameter (default is 0.5 elastic net)
#' @param remove_var_surrogate does the glmnet algorithm should learn on features in var_surrogate (default is TRUE).
#' @param bool_weight Should the glmnet function be weighted to balance the extrema populations (default is FALSE).
#' @param ... arguments to pass to pretty_cv.glmnet
#' @return A list
#' \itemize{
#'  \item glmnet_model - A list of three elements: the cv.glmnet fitted model, the coefficients of non zero variables and the vector of non zero coefficient variables.
#'  \item important_var - A vector with the variables used for the surrogate and the non zero variables.
#'  \item surrogate_quali - The surrogate_quali argument.
#' }
#' 
safe_selection <- function(df,
                           var_surrogate,
                           surrogate_quali,
                           threshold = 0.05,
                           alpha = 0.5,
                           remove_var_surrogate = TRUE,
                           bool_weight = FALSE,
                           ...){
  ## get matrix without var_surrogate and keep matrix of var_surrogates
  x_matrix <- df %>% select(-var_surrogate)
  surrogates_matrix <- df %>% select(var_surrogate)
  
  ## set x matrix for safe and remove vars used for building the surrogate if asked
  if(remove_var_surrogate){
    x_safe <- x_matrix
  } else {
    x_safe <- cbind(x_matrix, surrogates_matrix)
  }
  
  ## get the extremes
  y <- surrogate_quali[surrogate_quali != 3]
  x_glmnet <- as.matrix(x_safe[surrogate_quali != 3,])
  
  prop_extremes <- 1-table(y)/sum(table(y))
  
  ## set weight
  if(bool_weight){
    vec_weight <- prop_extremes[y+1]
  } else {vec_weight <- rep(1, length(y))}
  
  ## train model on extremes
  fit <- pretty_cv.glmnet(x_glmnet = x_glmnet,
                          y = y,
                          alpha = alpha,
                          weights = vec_weight,
                          standardize=TRUE,
                          ...)
  
  ## get the matrix with only non zero variables
  if(remove_var_surrogate){
    important_var <- c(var_surrogate, fit$nz_variables)
  } else {
    important_var <- fit$nz_variables
  }
  
  ## return result of model, matrix with only selected variables and surrogate quali
  return(list(glmnet_model = fit,
              important_var = important_var,
              surrogate_quali = surrogate_quali))
}