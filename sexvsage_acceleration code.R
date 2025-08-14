library(dplyr)
library(ggplot2)

# Join proteomics with rMEQ scoring data
agesex_plot <- proteomic_time_prediction_results %>%
  inner_join(
    rmeq_scoring %>%
      select(eid, rmeq_score, rmeq_chronotype, age, sex),
    by = "eid"
  ) %>%
  mutate(
    gap = pred_lasso - time_day,
    rmeq_chronotype = factor(
      rmeq_chronotype,
      levels = c("Definitely morning", "Rather morning", "Neither type",
                 "Rather evening", "Definitely evening")
    )
  ) %>%
  filter(!is.na(age), !is.na(sex), !is.na(rmeq_score), !is.na(gap))

#  rMEQ score vs Age 
ggplot(agesex_plot, aes(x = age, y = rmeq_score, colour = sex)) +
  #geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "rMEQ score vs Age",
    x = "Age (years)",
    y = "rMEQ score"
  ) +
  theme_minimal()

# Blood-based acceleration vs Age 
ggplot(agesex_plot, aes(x = age, y = gap, colour = sex)) +
  # geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Blood-based acceleration vs Age",
    x = "Age (years)",
    y = "Acceleration (gap)"
  ) +
  theme_minimal()
