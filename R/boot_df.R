#' boot_df
#'
#' @description Sample rows with replacement from a matrix
#'
#'@param x_matrix matrix to perform sampling on
#'@param y_sur The numeric vector of the qualitative surrogate.
#'@param ID The patient ID
#'@param size size of matrix returned
#'@param seed seed for sampling
#'@param prob Vector for weight sampling
#'
#'@return A list with the sampled explanatory matrix and the sampled qualitative surrogate (y_sur)
boot_df <- function(x_matrix,
                    y_sur,
                    ID = NULL,
                    size = 10^5,
                    seed = 1,
                    prob = NULL){
        ## sample
        set.seed(seed)
        if(is.null(prob)){
                sample_105 <- sample(1:nrow(x_matrix), size = size, replace = TRUE)
        } else {
                sample_105 <- sample(1:nrow(x_matrix), size = size, replace = TRUE, prob = prob)
        }
        ## build df
        X_boot <- x_matrix[sample_105,]
        Y_boot <- y_sur[sample_105]
        if(!is.null(ID)){
                ID_boot <- ID[sample_105]
                return(list(X_boot = X_boot,
                            Y_boot = Y_boot,
                            ID_boot = ID_boot))
        }
        ## return result
        return(list(X_boot = X_boot,
                    Y_boot = Y_boot))
}
