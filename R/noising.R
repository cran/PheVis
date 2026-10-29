#' noising
#'
#' @description Noise a matrix
#' 
#'@param X_boot matrix to perform noise on
#'@param p amount of noise
#'
#'@return A noised matrix
noising <- function(X_boot, p = .3){
  ## get mean for each col
  column_mean <- colMeans(X_boot)
  ## get rows where noise is introduced
  mat <- matrix(round(stats::runif(prod(dim(X_boot))) < p),
                ncol = ncol(X_boot))
  ## work on copy
  X_boot_noise <- X_boot
  ## assign noise
  for(col in 1:ncol(X_boot)){
    bool_col <- mat[,col] == 1
    X_boot_noise[bool_col,col] <- column_mean[col]
  }
  return(X_boot_noise)
}
