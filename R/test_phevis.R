#' test_phevis
#'
#' @param train_param Parameters for the model training (variables used, main ICD and CUIS, half_life, gold standard, omega). Usually obtained from train_phevis() function.
#' @param df_test The dataframe on which to make the prediction.
#' @param surparam The parameters used to compute the surrogate. Usually obtained by train_phevis() function.
#' @param model The random intercept logistic regression. Usually obtained by train_phevis() function.
#' @param START_DATE Column name of the time column. The time column should be numeric
#' @param PATIENT_NUM Column name of the patient id column.
#' @param ENCOUNTER_NUM Column name of the encounter id column.
#' 
#' @return A dataframe with the predictions.
#' 
#' @examples 
#' \donttest{library(dplyr)
#' library(PRROC)
#' PheVis::data_phevis
#' PheVis::data_perf
#' 
#' var_vec <- c(paste0("var",1:10), "mainCUI", "mainICD")
#' main_icd <- "mainICD"
#' main_cui <- "mainCUI"
#' GS <- "PR_state"
#' half_life <- Inf
#' 
#' df <- data_phevis %>%
#'         mutate(ENCOUNTER_NUM = row_number(),
#'                time = round(as.numeric(time)))
#' 
#' trainsize <- 0.8*length(unique(df$subject))
#' trainid <- sample(x = unique(df$subject), size = trainsize)
#' testid <- unique(df$subject)[!unique(df$subject) %in% trainid]
#' 
#' df_train <- as.data.frame(df[df$subject %in% trainid,])
#' df_test <- as.data.frame(df[df$subject %in% testid,])
#' 
#' ##### train and test model #####
#' train_model <- PheVis::train_phevis(half_life = half_life,
#'                                     df = df_train,
#'                                     START_DATE = "time",
#'                                     PATIENT_NUM = "subject",
#'                                     ENCOUNTER_NUM = "ENCOUNTER_NUM",
#'                                     var_vec = var_vec,
#'                                     main_icd = main_icd,
#'                                     main_cui = main_cui)
#' 
#' test_perf <- PheVis::test_phevis(train_param = train_model$train_param,
#'                                  df_test = df_test,
#'                                  START_DATE = "time",
#'                                  PATIENT_NUM = "subject",
#'                                  ENCOUNTER_NUM = "ENCOUNTER_NUM",
#'                                  surparam = train_model$surparam,
#'                                  model = train_model$model)
#' 
#' pr_curve <-PRROC::pr.curve(scores.class0 = test_perf$df_result$PREDICTION,
#'                            weights.class0 = df_test$PR_state)
#' 
#' roc_curve <- PRROC::roc.curve(scores.class0 = test_perf$df_result$PREDICTION,
#'                               weights.class0 = df_test$PR_state)}
#' 
#' @export
test_phevis <- function(train_param,
                        df_test,
                        surparam,
                        model,
                        START_DATE,
                        PATIENT_NUM,
                        ENCOUNTER_NUM){
        
        ### check user arguments
        check_arg <- check_arg_test_phevis(START_DATE = START_DATE,
                                           PATIENT_NUM = PATIENT_NUM,
                                           ENCOUNTER_NUM = ENCOUNTER_NUM,
                                           train_param = train_param,
                                           df_test = df_test,
                                           surparam = surparam,
                                           model = model)
        
        ### rename columns for convenience
        colnames(df_test)[colnames(df_test) == START_DATE] <- "START_DATE"
        colnames(df_test)[colnames(df_test) == PATIENT_NUM] <- "PATIENT_NUM"
        colnames(df_test)[colnames(df_test) == ENCOUNTER_NUM] <- "ENCOUNTER_NUM"
        
        ##### predict on test set
        ### rolling cumulative variables
        rolling_cum_test <- matrix_exp_smooth(half_life = train_param$half_life,
                                              df = df_test[,train_param$var_vec],
                                              date = df_test$START_DATE,
                                              patient_id = df_test$PATIENT_NUM,
                                              encounter_id = df_test$ENCOUNTER_NUM)
        
        sur_test <- fct_surrogate_quanti(main_icd = train_param$main_icd,
                                         main_cui = train_param$main_cui,
                                         omega = train_param$omega,
                                         df = df_test,
                                         date = "START_DATE",
                                         patient_id = "PATIENT_NUM",
                                         encounter_id = "ENCOUNTER_NUM",
                                         half_life = train_param$half_life,
                                         param = surparam)
        
        rolling_cum_test <- rolling_cum_test %>%
                rename(START_DATE = .data$date,
                       PATIENT_NUM = .data$patient_id,
                       ENCOUNTER_NUM = .data$encounter_id) %>%
                select(-.data$lag_date,
                       -.data$diff_date)
        
        ### merge all dataframe and select variables
        df_all_test <- rolling_cum_test %>%
                left_join(sur_test$table)
        
        ### get prediction
        vec_predictions <- pred_lme4model(fe.model = model$fixedEffect,
                                          df = df_all_test)
        df_testpr_result <- df_all_test %>%
                select(.data$PATIENT_NUM,
                       .data$ENCOUNTER_NUM,
                       .data$START_DATE,
                       .data$SUR_QUANTI,
                       .data$SUR_QUALI) %>%
                mutate(PREDICTION = vec_predictions)
        
        ### add the gold-standard if provided
        if(!is.null(train_param$GS)){
                df_testpr_result <- df_testpr_result %>%
                        left_join(df_test[,c("ENCOUNTER_NUM", train_param$GS)])
        }
        
        return(list(df_result = df_testpr_result,
                    df_pred = df_all_test))
}