#' phenorm_longit_simpl
#' 
#' @description 'PheNorm' like function adapted to longitudinal data.
#'
#' @param df dataframe
#' @param var_surrogate variables used for building the surrogates
#' @param surrogates_quali numeric vector of the qualitative surrogate
#' @param id_rnd ID for random effect
#' @param rf should pseudo-labellisation with random forest be used (default is FALSE)
#' @param ntree number of tree for \code{randomforest} (default is 100)
#' @param bool_weight should the sampling probability balance the number of positive and negative extrema.
#' @param p.noise percentage of noise introduced during the noising step
#' @param size minimum size of sampling
#' @param bool_SAFE A boolean. If TRUE, SAFE selection is done, else it is not (default is TRUE)
#' 
#' @return A list with the logistic model, the random forest model, the variables selected for prediction and the predictions
phenorm_longit_simpl <- function(df,
                                 var_surrogate,
                                 surrogates_quali,
                                 id_rnd,
                                 rf = FALSE,
                                 ntree = 100,
                                 bool_weight = FALSE,
                                 p.noise = .3,
                                 bool_SAFE = TRUE,
                                 size = 10^5){
        ## df without id
        df.unid <- df %>% select(-id_rnd)
        
        colmin <- apply(df.unid, 2, min)
        
        fct_min <- function(x) min(x) > 0
        ## log transformation
        df_transf <- df.unid %>%
                mutate_if(fct_min, .funs = function(x) log(1+x))
        
        ## train lasso on surrogates quali
        if(bool_SAFE){
                lasso_sur <- safe_selection(df = df_transf,
                                            var_surrogate = var_surrogate,
                                            surrogate_quali = surrogates_quali,
                                            bool_weight = bool_weight)
                # selected variables
                var.prediction <- lasso_sur$important_var
        } else {
                var.prediction <- colnames(df_transf)
        }
        
        ## select df
        df_select <- df_transf %>% select(var.prediction)
        
        x_matrix <- as.data.frame(df_select)
        
        rf_model <- NULL
        if(rf){
                x_rf <- as.data.frame(x_matrix)
                rf_model <- randomForest::randomForest(x = x_rf[surrogates_quali != 3,],
                                                       y = as.factor(surrogates_quali[surrogates_quali != 3]),
                                                       ntree = ntree)
                sur_quali_rf <- predict(rf_model, x_rf)
                
                surrogates_quali <- as.numeric(as.character(sur_quali_rf))
        }
        
        ## phenorm simpl longit
        phenorm <- phenorm_longit_fit(x_matrix = x_matrix,
                                      y_sur = surrogates_quali,
                                      p.noise = p.noise,
                                      size = size,
                                      ID = df[,id_rnd])
        ## return perf
        return(list(model = phenorm,
                    var.prediction = var.prediction,
                    rf_model = rf_model
        ))
}
