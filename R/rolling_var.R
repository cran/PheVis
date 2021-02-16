#' @title rolling_var
#' @description Compute rolling variables (last visit, last 5 visits, last month and last year)
#' 
#' @param id Patient id numeric vector
#' @param var Variable numeric vector
#' @param start_date Time numeric vector
#' @param id_encounter Encounter id vector
#'
#' @return A dataframe containing the rolling variables.
rolling_var <- function(id, var, start_date, id_encounter){
  . <- NULL
  ## set year and month sizes
  win_size_month = 30
  win_size_year = 365
  ## create df
  df <- data.frame(id = id,
                   id_encounter = id_encounter,
                   var = var,
                   start_date = start_date)
  
  ## compute rolling var
  df_rolling <- df %>%
    filter(!is.na(df$start_date)) %>%
    group_by(.data$id) %>%
    arrange(start_date) %>%
    mutate(last_vis = lag(.data$var),
           last_5vis = cum_lag(x = .data$var, n_lag = 5),
           cum = cumsum(.data$var)) %>%
    replace(is.na(.),0) %>%
    arrange(.data$id, .data$start_date)
  
  df_roll_time <- roll_time_sum(id = id,
                                id_encounter = id_encounter,
                                var = var,
                                start_date = start_date,
                                win_size1 = win_size_month,
                                win_size2 = win_size_year)
  
  result <- df_rolling %>%
    inner_join(df_roll_time)
  
  return(result)
}
