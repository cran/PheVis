## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(PheVis)
library(dplyr)
library(knitr)
library(ggplot2)

data("data_phevis")

kable(head(data_phevis[,1:7]))

## -----------------------------------------------------------------------------
df <- data_phevis %>%
  mutate(ENCOUNTER_NUM = row_number(),
         time = round(as.numeric(time)))

set.seed(1)

trainsize <- 0.8*length(unique(df$subject))
trainid <- sample(x = unique(df$subject), size = trainsize)
testid <- unique(df$subject)[!unique(df$subject) %in% trainid]

df_train <- as.data.frame(df[df$subject %in% trainid,])
df_test <- as.data.frame(df[df$subject %in% testid,])

## -----------------------------------------------------------------------------
var_vec <- c(paste0("var",1:10), "mainCUI", "mainICD")
main_icd <- "mainICD"
main_cui <- "mainCUI"
GS <- "PR_state"
half_life <- Inf

## -----------------------------------------------------------------------------
train_model <- PheVis::train_phevis(half_life = half_life,
                                    df = df_train,
                                    START_DATE = "time",
                                    PATIENT_NUM = "subject",
                                    ENCOUNTER_NUM = "ENCOUNTER_NUM",
                                    var_vec = var_vec,
                                    main_icd = main_icd,
                                    main_cui = main_cui)

## -----------------------------------------------------------------------------
test_model <- PheVis::test_phevis(train_param = train_model$train_param,
                                  df_test = df_test,
                                  START_DATE = "time",
                                  PATIENT_NUM = "subject",
                                  ENCOUNTER_NUM = "ENCOUNTER_NUM",
                                  surparam = train_model$surparam,
                                  model = train_model$model)

## -----------------------------------------------------------------------------
train_model$surparam

## -----------------------------------------------------------------------------
train_model$model

## -----------------------------------------------------------------------------
head(train_model$df_train_result)

## -----------------------------------------------------------------------------
train_model$train_param

## -----------------------------------------------------------------------------
head(train_model$df_x_train[,c(1:2, 14:15, 29:30)])

## -----------------------------------------------------------------------------
head(test_model$df_result)

## -----------------------------------------------------------------------------
head(test_model$df_pred[,c(1:2, 14:15, 29:30)])

## -----------------------------------------------------------------------------
df_plot <- test_model$df_result %>%
  left_join(df_test) %>%
  filter(PATIENT_NUM %in% c(18, 23, 26, 32))

PheVis::ggindividual_plot(subject = df_plot$PATIENT_NUM,
                          time = df_plot$START_DATE,
                          gold_standard = df_plot$PR_state,
                          prediction = df_plot$PREDICTION)

## ----fig.width=6--------------------------------------------------------------
pr_curve <-PRROC::pr.curve(scores.class0 = test_model$df_result$PREDICTION,
                           weights.class0 = df_test$PR_state,
                           curve = TRUE)

plot(pr_curve)

## ----fig.width=6--------------------------------------------------------------
roc_curve <- PRROC::roc.curve(scores.class0 = test_model$df_result$PREDICTION,
                              weights.class0 = df_test$PR_state,
                              curve = TRUE)
plot(roc_curve)

