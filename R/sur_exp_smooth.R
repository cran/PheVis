#' sur_exp_smooth
#' 
#' @description Function to cumulate surrogate with exponential decay
#' @param half_life Duration of cumulation. For a chronic disease you might chose Inf, for acute disease you might chose the duration of the disease.
#' @param sur The quantitative surrogate.
#' @param date A numeric vector of time of days unit.
#' @param patient_id Vector of patient ID
#' @param encounter_id Vector of encounter ID
#' 
#' @return A dataframe with the cumulated surrogate.
sur_exp_smooth <- function(half_life,
                           sur,
                           date,
                           patient_id,
                           encounter_id){
        lambda <- log(2)/half_life
        
        df_norm_exp <- data.frame(sur = sur,
                                  date = date,
                                  patient_id = patient_id,
                                  encounter_id = encounter_id)
        
        df_exp_sur <- df_norm_exp %>%
                arrange(.data$patient_id, .data$date) %>%
                group_by(.data$patient_id) %>%
                mutate(lag_date = lag(.data$date),
                       lag_sur = lag(.data$sur)) %>%
                mutate(diff_date = as.numeric(.data$date-.data$lag_date),
                       Correct_value = NA,
                       lag_correct = NA)
        
        for(i in 1:nrow(df_exp_sur)){
                # compute corrected value
                correct_value <- df_exp_sur$sur[i]+df_exp_sur$lag_correct[i]*exp(-df_exp_sur$diff_date[i]*lambda)
                if(is.na(correct_value)) correct_value <- df_exp_sur$sur[i]
                # insert into dataframe
                df_exp_sur$Correct_value[i] <- correct_value
                if(!is.na(df_exp_sur$lag_date[i+1])) df_exp_sur$lag_correct[i+1] <- correct_value
        }
        
        return(df_exp_sur %>%
                       select(.data$patient_id,
                              .data$encounter_id,
                              .data$date,
                              .data$Correct_value))
}