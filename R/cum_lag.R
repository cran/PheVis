#' cum_lag
#' 
#' @description helpful function to cumulate information.
#' 
#' @param x numeric vector for which lag variable should be computed
#' @param n_lag size of lag window
#'
#' @return return numeric vector.
cum_lag <- function(x, n_lag){
  result <- 0
  for(i in 1:n_lag){
    result <- result + ifelse(is.na(lag(x, i)), 0, lag(x, i))
  }
  return(result)
}
