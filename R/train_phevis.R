#' train_phevis
#' 
#' @description Global function to train phevis model.
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
#' @return A list
#' \itemize{
#'  \item surparam - the parameters used to compute the surrogate
#'  \item model - the random intercept logistic regression
#'  \item df_train_result - the \code{data.frame} containing the output predictions
#'  \item train_param - parameters for the model training (variables used, main ICD and CUIS, half_life, gold standard)
#' }
#' 
#' @examples
#' \donttest{library(dplyr)
#' PheVis::data_phevis
#' df <- data_phevis %>%
#'          mutate(ENCOUNTER_NUM = row_number(),
#'          time = round(as.numeric(time)))
#' model <- PheVis::train_phevis(half_life = Inf,
#'          df = df,
#'          START_DATE = "time",
#'          PATIENT_NUM = "subject",
#'          ENCOUNTER_NUM = "ENCOUNTER_NUM",
#'          var_vec = c(paste0("var",1:10), "mainCUI", "mainICD"),
#'          main_icd = "mainICD",
#'          main_cui = "mainCUI")}
#' 
#' @export
train_phevis <- function(half_life,
                         df,
                         START_DATE,
                         PATIENT_NUM,
                         ENCOUNTER_NUM,
                         var_vec,
                         main_icd,
                         main_cui,
                         rf = TRUE,
                         p.noise = 0.3,
                         bool_SAFE = TRUE,
                         omega = 2,
                         GS = NULL){
        
        ### check user arguments
        check_arg <- check_arg_train_phevis(half_life = half_life,
                                            df = df,
                                            START_DATE = START_DATE,
                                            PATIENT_NUM = PATIENT_NUM,
                                            ENCOUNTER_NUM = ENCOUNTER_NUM,
                                            var_vec = var_vec,
                                            main_icd = main_icd,
                                            main_cui = main_cui,
                                            rf = rf,
                                            p.noise = p.noise,
                                            bool_SAFE = bool_SAFE,
                                            omega = omega,
                                            GS = GS)
        
        ### rename columns for convenience
        colnames(df)[colnames(df) == START_DATE] <- "START_DATE"
        colnames(df)[colnames(df) == PATIENT_NUM] <- "PATIENT_NUM"
        colnames(df)[colnames(df) == ENCOUNTER_NUM] <- "ENCOUNTER_NUM"
        
        ### build the surrogate
        sur_train <- fct_surrogate_quanti(main_icd = main_icd,
                                          main_cui = main_cui,
                                          df = df,
                                          omega = omega,
                                          date = "START_DATE",
                                          patient_id = "PATIENT_NUM",
                                          encounter_id = "ENCOUNTER_NUM",
                                          half_life = half_life)
        
        ### cumultate variables if needed
        if(half_life != 0){
                # cumulate variables
                rolling_cum <- matrix_exp_smooth(half_life = half_life,
                                                 df = df[,var_vec],
                                                 date = df$START_DATE,
                                                 patient_id = df$PATIENT_NUM,
                                                 encounter_id = df$ENCOUNTER_NUM) %>%
                        rename(START_DATE = .data$date,
                               PATIENT_NUM = .data$patient_id,
                               ENCOUNTER_NUM = .data$encounter_id) %>%
                        select(-.data$lag_date,
                               -.data$diff_date)
                
                if(half_life == Inf){
                        df_all_train <- rolling_cum %>%
                                left_join(sur_train$table)
                } else {
                        # if half_life != Inf => no cum surrogate variables
                        df_all_train <- rolling_cum %>%
                                left_join(sur_train$table) %>%
                                select(-.data$cum)
                }
                
        } else {
                # if half_life = 0 => no cumulated variables
                df_all_train <- sur_train$table %>%
                        left_join(df[,c(var_vec, "PATIENT_NUM", "ENCOUNTER_NUM")]) %>%
                        select(-.data$cum) %>%
                        ungroup()
        }
        
        ### select only variables used in the X matrix
        df_x_train <- df_all_train %>%
                select(-.data$SUR_QUALI,
                       -.data$SUR_QUANTI,
                       -.data$ENCOUNTER_NUM,
                       -.data$START_DATE) %>%
                as.data.frame()
        
        var.surrogate <- unlist(c(main_cui, main_icd), use.names = FALSE)
        
        ### Train the model for probability estimation with pseudo label and noising denoising
        model <- phenorm_longit_simpl(df = df_x_train,
                                      var_surrogate = var.surrogate,
                                      surrogates_quali = df_all_train$SUR_QUALI,
                                      id_rnd = "PATIENT_NUM",
                                      size = nrow(df_x_train),
                                      ntree = 50,
                                      p.noise = p.noise,
                                      bool_SAFE = bool_SAFE,
                                      rf = rf)
        
        ### put the results in dataframe
        df_train_result <- data.frame(ENCOUNTER_NUM = df_all_train$ENCOUNTER_NUM,
                                      PRED = model$model$prediction,
                                      SUR_QUANTI = df_all_train$SUR_QUANTI,
                                      SUR_QUALI = df_all_train$SUR_QUALI)
        
        ### add the gold-standard if provided
        if(!is.null(GS)){
                df_train_result <- df_train_result %>%
                        left_join(df[,c("ENCOUNTER_NUM", GS)])
        }
        ### remove predictions for lighter object
        model$model$prediction <- NULL
        
        ### result list
        result <- list(surparam = sur_train$param,
                       model = model$model,
                       df_train_result = df_train_result,
                       train_param = list(half_life = half_life,
                                          omega = omega,
                                          var_vec = var_vec,
                                          main_icd = main_icd,
                                          main_cui = main_cui,
                                          GS = GS),
                       df_x_train = df_x_train)
        return(result)
}
