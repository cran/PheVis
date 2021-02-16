#' phenorm_longit_fit
#' 
#' @description  Apply simplified 'PheNorm' algorithm on longitudinal data with bootstrap and noise.
#'
#' @param x_matrix x matrix to sample, noise and predict on
#' @param y_sur surrogate with 3 values (0 and 1 the extremes and 3 middle patients)
#' @param ID Vector of patient ID
#' @param size size of sampling. default is 10^5
#' @param seed seed. default is 1.
#' @param p.noise noise probability parameter. default is .3.
#' @param do_sampling should algorithm do sampling. default is TRUE.
#' @param do_noise should algorithm do noise. default is TRUE.
#' @param prob sampling probability during noising denoising step
#' @param calc.prob should the `prob` argument be calculated
#' @param nAGQ glmer parameter
#' @param glmer.control glmer parameter
#' 
#' @return A list with the fixed effects, the predicted responses and the model used (mixed effect or logistic regression)
#' 
#' @importFrom stats glm
phenorm_longit_fit <- function(x_matrix,
                               y_sur,
                               ID,
                               size = 10^5,
                               seed = 1,
                               p.noise = .3,
                               do_sampling = TRUE,
                               do_noise = TRUE,
                               prob = NULL,
                               calc.prob = TRUE,
                               nAGQ = 0,
                               glmer.control = glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=2e5))){
        
        ## phenorm simpl longit
        ## select extreme patients
        x_extreme <- x_matrix[y_sur != 3,]
        y_extreme <- y_sur[y_sur != 3]
        
        ## weight calculation for sampling
        if(calc.prob){
                tab.sur <- table(y_extreme)
                w.vec <- c(1-tab.sur[1]/sum(tab.sur), 1-tab.sur[2]/sum(tab.sur))
                prob <- w.vec[y_extreme+1]
        }
        
        ## boot strap if asked
        if(do_sampling){
                XY_boot <- boot_df(x_matrix = x_extreme,
                                   y_sur = y_extreme,
                                   ID = ID,
                                   size = size,
                                   seed = seed,
                                   prob = prob)
                x_extreme <- XY_boot$X_boot
                y_extreme <- XY_boot$Y_boot
                ID_extreme <- XY_boot$ID_boot
        }
        
        ## noise if asked
        if(do_noise){
                x_extreme <- noising(X_boot = x_extreme,
                                     p = p.noise)
        }
        
        ## merge df
        xy_fit <- cbind(y_sur = as.factor(y_extreme),
                        x_extreme,
                        ID = as.factor(ID_extreme))
        
        ## train model
        # set formulas
        baseTextFormula <- paste0("y_sur ~ ",
                              paste0(collapse = "+",
                                     colnames(x_extreme)))
        baseFormula <- as.formula(baseTextFormula)
        mixedFormula <- as.formula(paste0(baseTextFormula, "+ (1|ID)"))
        
        # try mixed model, if error, run simple logistic regression model
        model <- tryCatch(expr = {
                fit <- lme4::glmer(formula = mixedFormula,
                                   data = xy_fit,
                                   family = "binomial",
                                   nAGQ = nAGQ,
                                   control = glmer.control)
                ## fixed effects
                fixedEffect <- fixef(fit)
                ## predict on x_matrix
                prediction <- pred_lme4model(fe.model = fixedEffect,
                                             df = x_matrix)
                
                return(list(fixedEffect = fixedEffect,
                            prediction = prediction,
                            model = "glmer"))
        },
        error=function(err) {
                warning("Mixed model failed -- running simple logistic regression instead of mixed model")
                fit <- glm(formula = baseFormula,
                           data = xy_fit,
                           family = "binomial")
                fixedEffect <- coef(fit)
                
                prediction <- pred_lme4model(fe.model = fixedEffect,
                                             df = x_matrix)
                
                return(list(fixedEffect = fixedEffect,
                            prediction = prediction,
                            model = "glm"))
        })
        
        ## return result
        return(model)
}
