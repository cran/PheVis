#' norm_var
#' @description Standardize a numeric variable
#' @param x A numeric variable
#'
#' @return The standardized variable
norm_var <- function(x){
        result <- (x-mean(x))/sd(x)
        result <- ifelse(is.na(result), 0, result)
        return(result)
}