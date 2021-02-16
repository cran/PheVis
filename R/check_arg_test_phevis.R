#' check_arg_test_phevis
#' 
#' @description Function to check arguments passed to test_phevis()
#' @param train_param Parameters for the model training (variables used, main ICD and CUIS, half_life, gold standard, omega). Usually obtained from train_phevis() function.
#' @param df_test The dataframe on which to make the prediction.
#' @param surparam The parameters used to compute the surrogate. Usually obtained by train_phevis() function.
#' @param model The random intercept logistic regression. Usually obtained by train_phevis() function.
#' @param START_DATE Column name of the time column. The time column should be numeric
#' @param PATIENT_NUM Column name of the patient id column.
#' @param ENCOUNTER_NUM Column name of the encounter id column.
#' 
#' @return No return value, stop the code execution if one condition is not met.
check_arg_test_phevis <- function(train_param,
                                  df_test,
                                  surparam,
                                  model,
                                  START_DATE,
                                  PATIENT_NUM,
                                   ENCOUNTER_NUM){
        ### START_DATE
        if(!is.character(START_DATE)) stop("START_DATE should be character")
        if(length(START_DATE) != 1) stop("START_DATE should be of length 1")
        
        ### PATIENT_NUM
        if(!is.character(PATIENT_NUM)) stop("PATIENT_NUM should be character")
        if(length(PATIENT_NUM) != 1) stop("PATIENT_NUM should be of length 1")
        
        ### ENCOUNTER_NUM
        if(!is.character(ENCOUNTER_NUM)) stop("ENCOUNTER_NUM should be character")
        if(length(ENCOUNTER_NUM) != 1) stop("ENCOUNTER_NUM should be of length 1")
        
        ### test df_test
        if(!is.data.frame(df_test)) stop("df_test should be a data.frame object")
        
        ### test train_param
        if(!is.list(train_param)) stop("train_param should be a list")
        
        ### test surparam
        if(!is.list(surparam)) stop("surparam should be a list")
        
        ### test model
        if(!is.list(model)) stop("model should be a list")
        
        ### if everything is ok return message ok
        
        message("-- Arguments passed check -> PheVis begins --")
        
        return(NULL)
}