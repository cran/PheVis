test_that("test all pipeline is working", {
        set.seed(1)
        PheVis::data_phevis
        PheVis::data_perf

        var_vec <- c(paste0("var",1:10), "mainCUI", "mainICD")
        main_icd <- "mainICD"
        main_cui <- "mainCUI"
        GS <- "PR_state"
        half_life <- Inf

        df <- data_phevis %>%
          mutate(ENCOUNTER_NUM = row_number(),
                 time = round(as.numeric(time)))

        trainsize <- 0.8*length(unique(df$subject))
        trainid <- sample(x = unique(df$subject), size = trainsize)
        testid <- unique(df$subject)[!unique(df$subject) %in% trainid]

        df_train <- as.data.frame(df[df$subject %in% trainid,])
        df_test <- as.data.frame(df[df$subject %in% testid,])

        ##### train and test model #####
        train_model <- PheVis::train_phevis(half_life = half_life,
                                            df = df_train,
                                            START_DATE = "time",
                                            PATIENT_NUM = "subject",
                                            ENCOUNTER_NUM = "ENCOUNTER_NUM",
                                            var_vec = var_vec,
                                            main_icd = main_icd,
                                            main_cui = main_cui)

        test_perf <- PheVis::test_phevis(train_param = train_model$train_param,
                                         df_test = df_test,
                                         START_DATE = "time",
                                         PATIENT_NUM = "subject",
                                         ENCOUNTER_NUM = "ENCOUNTER_NUM",
                                         surparam = train_model$surparam,
                                         model = train_model$model)

        pr_curve <-PRROC::pr.curve(scores.class0 = test_perf$df_result$PREDICTION,
                                   weights.class0 = df_test$PR_state)

        roc_curve <- PRROC::roc.curve(scores.class0 = test_perf$df_result$PREDICTION,
                                      weights.class0 = df_test$PR_state)

        check_value <- data_perf - c(pr_curve$auc.integral, roc_curve$auc)
        
        ## tolerance
        eps <- if (capabilities("long.double"))
                sqrt(.Machine$double.eps) else
                        0.01
        
        expect_equal(check_value[1], 0, tolerance = eps)
        expect_equal(check_value[2], 0, tolerance = eps)
})
