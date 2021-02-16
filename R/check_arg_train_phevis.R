#' check_arg_train_phevis
#' 
#' @description Function to check arguments passed to train_phevis()
#' @param half_life Duration of cumulation. For a chronic disease you might chose Inf, for acute disease you might chose the duration of the disease.
#' @param df \code{data.frame} containing all the variables.
#' @param var_vec Explanatory variables used for the prediction, including the main variables.
#' @param main_icd Character vector of the column names of the main ICD codes.
#' @param main_cui Character vector of the column names of the main CUIs.
#' @param GS Character string corresponding to the name of the gold-standard variable (default is null for which a vector of 0 will be taken).
#' @param START_DATE Column name of the time column. The time column should be numeric
#' @param PATIENT_NUM Column name of the patient id column.
#' @param ENCOUNTER_NUM Column name of the encounter id column.
#' @param omega Constant for the extrema population definition (default is 2)
#' @param rf should pseudo-labellisation with random forest be used (default is true)
#' @param p.noise percentage of noise introduced during the noising step (default is 0.3)
#' @param bool_SAFE A boolean. If TRUE, SAFE selection is done, else it is not (default is TRUE)
#' 
#' @return No return value, stop the code execution if one condition is not met.
check_arg_train_phevis <- function(half_life,
                                   df,
                                   START_DATE,
                                   PATIENT_NUM,
                                   ENCOUNTER_NUM,
                                   var_vec,
                                   main_icd,
                                   main_cui,
                                   rf,
                                   p.noise,
                                   bool_SAFE,
                                   omega,
                                   GS){
        ### test half_life
        if(!is.numeric(half_life)) stop("half_life should be numeric")
        if(length(half_life) != 1) stop("half_life should be of length 1")
        
        ### test df
        if(!is.data.frame(df)) stop("df should be a data.frame object")
        
        ### START_DATE
        if(!is.character(START_DATE)) stop("START_DATE should be character")
        if(length(START_DATE) != 1) stop("START_DATE should be of length 1")

        ### PATIENT_NUM
        if(!is.character(PATIENT_NUM)) stop("PATIENT_NUM should be character")
        if(length(PATIENT_NUM) != 1) stop("PATIENT_NUM should be of length 1")
        
        ### ENCOUNTER_NUM
        if(!is.character(ENCOUNTER_NUM)) stop("ENCOUNTER_NUM should be character")
        if(length(ENCOUNTER_NUM) != 1) stop("ENCOUNTER_NUM should be of length 1")

        ### var_vec
        if(!is.character(var_vec)) stop("var_vec should be character")
        
        ### main_icd
        if(!is.character(var_vec)) stop("main_icd should be character")

        ### main_cui
        if(!is.character(main_cui)) stop("main_cui should be character")
        
        ### rf
        if(!is.logical(rf)) stop("rf should be character")
        if(length(rf) != 1) stop("rf should be of length 1")
        
        ### p.noise
        if(!is.numeric(p.noise)) stop("p.noise should be numeric")
        if(length(p.noise) != 1) stop("p.noise should be of length 1")
        if(p.noise > 1 | p.noise < 0) stop("p.noise should be between 0 and 1")
        
        ### bool_SAFE
        if(!is.logical(bool_SAFE)) stop("bool_SAFE should be boolean")
        if(length(bool_SAFE) != 1) stop("bool_SAFE should be of length 1")
        
        ### omega
        if(!is.numeric(omega)) stop("omega should be numeric")
        if(length(omega) != 1) stop("omega should be of length 1")
        
        ### GS
        if(!(is.character(GS)|is.null(GS))) stop("GS should be character")
        if(!length(GS) %in% c(0,1)) stop("GS should be of length 1")
        
        ### if everything is ok return message ok
        
        message("-- Arguments passed check -> PheVis begins --")

        return(NULL)
}