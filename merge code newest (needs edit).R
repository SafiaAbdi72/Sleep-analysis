# create chronotype labels for each source- sleep assessment
sleep_assess_chrono <- sleep_assessment %>%
  mutate(chronotype_assess = case_when(
    p1180_i0 == "Definitely a 'morning' person" ~ "Definitely morning",
    p1180_i0 == "More a 'morning' than 'evening' person" ~ "Rather morning",
    p1180_i0 == "More an 'evening' than a 'morning' person" ~ "Rather evening",
    p1180_i0 == "Definitely an 'evening' person" ~ "Definitely evening",
    TRUE ~ NA_character_
  )) %>%
  select(eid, chronotype_assess) %>%
  mutate(source = "Sleep Assessment")
# sleep online
sleep_online_chrono <- sleep_online %>%
  mutate(chronotype_online = case_when(
    p30429 == "Definitely a morning-type" ~ "Definitely morning",
    p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning",
    p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening",
    p30429 == "Definitely an evening-type" ~ "Definitely evening",
    TRUE ~ NA_character_
  )) %>%
  select(eid, chronotype_online) %>%
  mutate(source = "Sleep Online")
# rmeq based

rmeq_scoring <- rmeq_scoring %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 & rmeq_score <= 25 ~ "Definitely morning",
      rmeq_score >= 18 & rmeq_score <= 21 ~ "Rather morning",
      rmeq_score >= 12 & rmeq_score <= 17 ~ "Neither type",
      rmeq_score >= 8  & rmeq_score <= 11 ~ "Rather evening",
      rmeq_score >= 4  & rmeq_score <= 7  ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  )

# Add circadian metrics first
proteomic_data <- proteomic_time_prediction_results %>%
  mutate(
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )

# Join 1: Sleep Assessment
assess_sd <- proteomic_data %>%
  inner_join(sleep_assess_chrono, by = "eid") %>%
  mutate(Source = "Sleep Assessment", Chronotype = chronotype_assess)

# Join 2: Sleep Online
online_sd <- proteomic_data %>%
  inner_join(sleep_online_chrono, by = "eid") %>%
  mutate(Source = "Sleep Online", Chronotype = chronotype_online)

# Join 3: rMEQ-Based
rmeq_sd <- proteomic_data %>%
  inner_join(rmeq_scoring, by = "eid") %>%
  mutate(Source = "rMEQ-Based", Chronotype = rmeq_chronotype)
# Create rMEQ-Based chronotype column in rmeq_scoring
rmeq_scoring <- rmeq_scoring %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Neither type",
      rmeq_score >= 8  ~ "Rather evening",
      rmeq_score >= 4  ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  )

# Join with proteomic data and calculate disruption
rmeq_sd <- proteomic_data %>%
  inner_join(rmeq_scoring, by = "eid") %>%
  mutate(
    Source = "rMEQ-Based",
    Chronotype = rmeq_chronotype,
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )


# Join and compute disruption
online_sd <- proteomic_data %>%
  inner_join(sleep_online_chrono, by = "eid") %>%
  mutate(
    Source = "Sleep Online",
    Chronotype = chronotype_online,
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )

# Summary stats
online_summary <- online_sd %>%
  group_by(Source, Chronotype) %>%
  summarise(
    N = n(),
    prop = N / nrow(online_sd),
    mean_gap = mean(gap, na.rm = TRUE),
    sd_gap = sd(gap, na.rm = TRUE),
    mean_res_abs = mean(res_abs, na.rm = TRUE),
    sd_res_abs = sd(res_abs, na.rm = TRUE),
    .groups = "drop"
  )

# Join and compute disruption
assess_sd <- proteomic_data %>%
  inner_join(sleep_assess_chrono, by = "eid") %>%
  mutate(
    Source = "Sleep Assessment Centre",
    Chronotype = chronotype_assess,
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )

# Summary stats
assess_summary <- assess_sd %>%
  group_by(Source, Chronotype) %>%
  summarise(
    N = n(),
    prop = N / nrow(assess_sd),
    mean_gap = mean(gap, na.rm = TRUE),
    sd_gap = sd(gap, na.rm = TRUE),
    mean_res_abs = mean(res_abs, na.rm = TRUE),
    sd_res_abs = sd(res_abs, na.rm = TRUE),
    .groups = "drop"
  )

# Combine all summaries into one dataframe
combined_summary <- bind_rows(rmeq_summary, online_summary, assess_summary)

# Reorder levels of Chronotype for plotting consistency
combined_summary$Chronotype <- factor(
  combined_summary$Chronotype,
  levels = c("Definitely morning", "Rather morning", "Neither type", "Rather evening", "Definitely evening")
)

library(ggplot2)
library(dplyr)

# Combine all 3 summary datasets
combined_summary_by_source <- bind_rows(
  assess_summary,
  online_summary,
  rmeq_summary
)

# Get overall mean and SD per source (ignoring chronotype)
summary_by_source <- combined_summary_by_source %>%
  group_by(Source) %>%
  summarise(
    mean_gap = mean(mean_gap, na.rm = TRUE),
    sd_gap = mean(sd_gap, na.rm = TRUE),
    N = sum(N),
    .groups = "drop"
  )

# Plot
ggplot(summary_by_source, aes(x = Source, y = mean_gap, fill = Source)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_gap - sd_gap, ymax = mean_gap + sd_gap),
                width = 0.2, color = "black") +
  labs(
    title = "Mean Circadian Acceleration (gap) by Source",
    x = "Chronotype Source",
    y = "Mean gap (pred_lasso - time of day)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

