#' @title roll_time_sum
#' @description Compute the cumulated information of what happened in past month and past year.
#'
#' @param id Patient id numeric vector
#' @param id_encounter Encounter id vector
#' @param var Variable numeric vector
#' @param start_date Time numeric vector
#' @param win_size1 First window size (default is 30)
#' @param win_size2 Second window size (default is 365)
#' @param name1 name of first rolling var (default is "cum_month")
#' @param name2 name of second rolling var (default is "cum_year")
#'
#' @return A dataframe containing the rolling variables.
roll_time_sum <- function(id, id_encounter, var, start_date,
                          win_size1 = 30,
                          win_size2 = 365,
                          name1 = "cum_month",
                          name2 = "cum_year"){
  
  df <- data.frame(id = id,
                   id_encounter = id_encounter,
                   start_date = start_date,
                   var = var)
  
  if(is.null(win_size2)){
    result <- df %>%
      group_by(id) %>%
      arrange(id, start_date) %>%
      tidyr::complete(start_date = tidyr::full_seq(start_date,
                                                   period = 1),
                      fill = list(var = 0)) %>%
      mutate(cum_rolling1 = zoo::rollapplyr(zoo::as.zoo(var),
                                            width = win_size1,
                                            FUN = sum,
                                            partial = TRUE)) %>%
      tidyr::drop_na(id_encounter) %>%
      ungroup()
  } else {
    result <- df %>%
      group_by(id) %>%
      arrange(id, start_date) %>%
      tidyr::complete(start_date = tidyr::full_seq(start_date, period = 1),
                      fill = list(var = 0)) %>%
      mutate(cum_rolling1 = as.numeric(zoo::rollapplyr(data = zoo::as.zoo(var),
                                                       width = win_size1,
                                                       FUN = sum,
                                                       partial = TRUE))) %>%
      mutate(cum_rolling2 = as.numeric(zoo::rollapplyr(zoo::as.zoo(var),
                                                       width = win_size2,
                                                       FUN = sum,
                                                       partial = TRUE))) %>%
      tidyr::drop_na(id_encounter) %>%
      ungroup()
  }
  
  colnames(result)[colnames(result) == "cum_rolling1"] <- name1
  colnames(result)[colnames(result) == "cum_rolling2"] <- name2
  
  return(result)
}
