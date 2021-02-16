#' ggindividual_plot
#' 
#' @description Plot individual predictions.
#'
#' @param subject numeric vector subject id
#' @param time numeric vector time or date
#' @param gold_standard numeric vector of gold standard
#' @param prediction numeric vector of prediction
#'
#' @return a ggplot graph
#' 
#' @examples 
#' ggindividual_plot(subject = rep(1,10),
#'   time = 1:10,
#'   gold_standard = c(0,0,1,1,0,0,1,1,0,0),
#'   prediction = runif(n = 10, min = 0, max = 1))
#' 
#' @export
ggindividual_plot <- function(subject,
                              time,
                              gold_standard,
                              prediction){
  
  df_plot <- data.frame(subject = c(subject,subject),
                        time = c(time,time),
                        gs = c(rep("GS",length(subject)),
                               rep("Proba", length(subject))),
                        prediction = c(gold_standard,
                                       prediction))
  ### data.frame for the geom_ribbon
  df_ribbon <- df_plot %>%
    filter(.data$gs == "GS")
  
  df_ribbon2 <- df_ribbon %>%
    mutate(time = lead(.data$time)) %>%
    na.omit %>%
    bind_rows(df_ribbon)
  
  ### data.frame for the probabilities predictions
  df_proba <- df_plot %>%
    filter(.data$gs == "Proba")
  
  color_indiv_pred <- viridis::viridis(2)
  
  p <- ggplot() +
    geom_ribbon(aes(x = time,
                    ymin = 0,
                    ymax = prediction,
                    fill = "area"),
                data = df_ribbon2) +
    scale_fill_manual(name = "",
                      labels = "Gold standard",
                      values = color_indiv_pred[2]) +
    
    geom_step(data = df_proba,
              aes(x = time,
                  y = prediction,
                  color = "prediction"))+
    scale_color_manual(name = "",
                       labels = "Prediction",
                       values = color_indiv_pred[1])+
    
    geom_point(data = df_proba,
               aes(x = time,
                   y = -0.1,
                   shape = "Visit"))+
    scale_shape_manual(values = "|") +
    
    scale_y_continuous(breaks=c(0,1), limits = c(-0.2,1)) +
    labs(x = "Date", y = "Probability") +
    
    facet_grid(subject ~ .)+
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(p)
}
