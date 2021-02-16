#' pred_lme4model
#'
#'@description  function to predict probability from 'lme4' or 'glm' objects
#'
#'@param model lme4 model
#'@param df dataframe for prediction
#'@param fe.model the fixed effect of a model
#'
#'@return A vector of the predictions
#'
#'@importFrom stats na.omit
pred_lme4model <- function(model = NULL, fe.model = NULL, df){
        if(!is.null(model)){
                fe.model <- lme4::fixef(model)
        }
        
        if(anyNA(fe.model)){
                warning("Missing value in the predictor, not reliable prediction ...")
                fe.model <- na.omit(fe.model)
        }
        
        mat.predictions <- matrix(ncol = length(names(fe.model)),
                                  nrow = nrow(df))
        mat.predictions[,1] <- 1
        for(i in 2:ncol(mat.predictions)){
                if(names(fe.model)[i] %in% colnames(df)){
                        mat.predictions[,i] <- df[,names(fe.model)[i]]
                } else {mat.predictions[,i] <- 0}
        }
        colnames(mat.predictions) <- names(fe.model)
        
        linear.predict <- mat.predictions %*% fe.model
        exp.linear.predict <- exp(linear.predict)
        proba.predicted <- exp.linear.predict/(1+exp.linear.predict)
        ## correct numerical approximation if exponential is equal to Infinity (generate NaN otherwise)
        proba.predicted[exp.linear.predict == Inf] <- 1
        
        return(proba.predicted)
}