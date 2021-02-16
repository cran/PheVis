#' matrix_exp_smooth
#' @description Function to accumulate the information with exponential decay.
#' @param half_life Duration of accumulation. For a chronic disease you might chose Inf, for acute disease you might chose the duration of the disease.
#' @param df Dataframe of the explanatory variables.
#' @param date Vector of date. The date should be in a numeric format.
#' @param patient_id The vector of patient id
#' @param encounter_id The vector of visit id
#' 
#' @return A data.frame object with both the raw variables and the accumulated ones.
matrix_exp_smooth <- function(half_life,
                              df,
                              date,
                              patient_id,
                              encounter_id){
  lambda <- log(2)/half_life
  
  # dataframe before correction
  df_initial <- data.frame(df,date,patient_id,encounter_id)
  # compute intermediate step (delai_ij)
  df_delai <- df_initial %>%
    arrange(.data$patient_id,.data$date) %>%
    group_by(.data$patient_id) %>%
    mutate(lag_date = lag(.data$date),
           diff_date = as.numeric(.data$date-.data$lag_date)) %>%
    ungroup() %>%
    as.data.frame()
  # prepare variables before calculations
  mat_to_correct <- as.matrix(df_delai[,1:ncol(df)])
  diff_date <- df_delai$diff_date
  # compute correction
  correct_matrix <- expcorrectC(mat = mat_to_correct,
                                diffdate = diff_date,
                                lambda = lambda)
  colnames(correct_matrix) <- paste0(colnames(mat_to_correct), "_cum")
  # merge all dataframe
  result <- cbind(df_delai, correct_matrix)
  return(result)
}
