#' PheVis: A package for visit phenotyping.
#'
#' @description Package implementing the PheVis algorithm for which a paper is in progress. The idea is to provide an unsupervised tool able to phenotype easily multiple phenotypes without chart review.
#' @docType package
#' @name PheVis
#' @import Rcpp dplyr glmnet lme4 zoo purrr tidyr viridis knitr ggplot2
#' @importFrom randomForest randomForest
#' @importFrom stats as.formula predict quantile sd coef
#' @importFrom Rcpp evalCpp
#' @useDynLib PheVis
NULL