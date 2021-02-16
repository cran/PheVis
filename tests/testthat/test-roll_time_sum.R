test_that("roll_time_sum", {
        df <- data.frame(id = c(rep(1,3), rep(2,4)),
                         id_encounter = 1:7,
                         start_date = c(1,7,90, 2,6,80,90),
                         var = c(0,1,0, 0,2,1,0))
        expected_vec1 <- c(0,1,0, 0,2,1,1)
        expected_vec2 <- c(0,1,1, 0,2,3,3)
        
        result <- roll_time_sum(id = df$id,
                                id_encounter = df$id_encounter,
                                var = df$var,
                                start_date = df$start_date)
        expect_equal(sum((result$cum_month - expected_vec1)^2), 0)
        expect_equal(sum((result$cum_year - expected_vec2)^2), 0)
})
