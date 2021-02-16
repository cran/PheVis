#' build_quali
#'
#' @param x A numeric vector
#' @param p The lower quantile
#' @param q The upper quantile
#'
#' @return The qualitative surrogate (x in three categories) defining the extrema populations
build_quali <- function(x, p, q){
        ### build surrogate quali
        surrogate_quanti_quantile <- quantile(x, p = c(p,q))
        # default assign 3
        surrogate_quali <- rep(3, length(x))
        # assign 0 to rows below lower threshold and 1 to rows above upper threshold
        surrogate_quali[x <= surrogate_quanti_quantile[1]] <- 0
        surrogate_quali[x >= surrogate_quanti_quantile[2]] <- 1
        return(surrogate_quali)
}