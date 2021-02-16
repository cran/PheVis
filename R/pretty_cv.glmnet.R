#' pretty_cv.glmnet
#' 
#' @description Train a 'glmnet' with cross validation (cv) model and return convenient results (model and results with non zero coefficients)
#' 
#' @param x_glmnet Independent variable matrix (X)
#' @param y Dependent variable vector (Y)
#' @param alpha alpha parameter of glmnet (default = 1)
#' @param family family parameter of glmnet (default = "binomial")
#' @param s lambda chosen from cv.glmnet (default = "lambda.1se")
#' @param weights glmnet parameter
#' @param ... additional parameters passed to glmnet
#' 
#' @return A list with the model, the coefficient associated with variables and the selected variables.
pretty_cv.glmnet <- function(x_glmnet,
                             y,
                             alpha = 1,
                             family = "binomial",
                             s = "lambda.1se",
                             weights = rep(1, nrow(x_glmnet)),
                             ...){
        ## fit the model
        model <- cv.glmnet(x = x_glmnet,
                           y = y,
                           weights = weights,
                           alpha = alpha,
                           family = family,
                           ...)
        
        ## get non zero coeff into df
        coeff <- as.numeric(coef(model, s=s))
        features <- rownames(coef(model, s=s))
        result <- data.frame(features = features[coeff != 0],
                             coefs    = coeff[coeff != 0])
        
        ## return results
        return(list(model = model,
                    result = result,
                    nz_variables = as.character(result$features[-1])))
}