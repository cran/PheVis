#' build_qantsur
#' 
#' @description build quantile threshold based on icd variables and omega constant
#'
#' @param df the dataframe containing the icd codes.
#' @param var.icd the main icd codes
#' @param omega the constant to define the extrema populations
#'
#' @return A numeric vector with the thresholds for the extrema populations.
build_qantsur <- function(df, var.icd, omega){
        if(length(var.icd) == 1){
                cim_pos <- table(df[,var.icd] > 0)
        } else {
                cim_pos <- table(rowSums(df[,colnames(df) %in% var.icd]) > 0)
        }
        p.sur <- cim_pos[2]/cim_pos[1]/omega
        q.sur <- 1-p.sur
        threshold <- c("p.sur" = as.numeric(p.sur), "q.sur" = as.numeric(q.sur))
        return(threshold)
}