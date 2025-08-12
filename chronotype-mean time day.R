# mean time of day attended clinic per chronotype
mean_time_assess <- proteomic_time_prediction_results %>% inner_join(sleep_assess_chrono, by = "eid") %>% group_by(chronotype_assess) %>% summarise(mean_time_day = mean(time_day, na.rm = TRUE),
              sd_time_day = sd(time_day, na.rm = TRUE), n = n()
) %>%
arrange(mean_time_day)
mean_time_assess
