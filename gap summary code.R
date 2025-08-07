
library(dplyr)
library(ggplot2)

#  1. Join each chronotype source to proteomic data and calculate gap/res_abs 



# Sleep Assessment
gap_assess <- proteomic_time_prediction_results %>%
  inner_join(sleep_assessment %>%
               filter(p1180_i0 != "") %>%
               select(eid, p1180_i0 ), by = "eid") %>%
  mutate(chronotype = case_when(p1180_i0 == "Definitely a 'morning' person" ~ "Definitely morning", p1180_i0 == "More a 'morning' than 'evening' person" ~ "Rather morning", p1180_i0 == "More an 'evening' than a 'morning' person" ~ "Rather evening", p1180_i0 == "Definitely an 'evening' person" ~ "Definitely evening",
                                TRUE ~ NA_character_)) %>%
  mutate(Source = "Assessment Centre",
         gap = pred_lasso - time_day,
         res_abs = abs(gap))

# Sleep Online
gap_online <- proteomic_time_prediction_results %>%
  inner_join(sleep_online %>%
               filter(p30429 != "") %>%
               select(eid, p30429), by = "eid") %>%
  mutate(chronotype = case_when(p30429 == "Definitely a morning-type" ~ "Definitely morning", p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning", p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening", p30429 == "Definitely an evening-type" ~ "Definitely evening",
                                TRUE ~ NA_character_)) %>%
  mutate(Source = "Sleep Online",
         gap = pred_lasso - time_day,
         res_abs = abs(gap))

# rMEQ-Based
gap_rmeq <- proteomic_time_prediction_results %>%
  inner_join(rmeq_scoring %>%
               filter(rmeq_score != "") %>%
               select(eid, rmeq_score), by = "eid") %>%
  mutate(chronotype = case_when(rmeq_score >= 22 & rmeq_score <= 25 ~ "Definitely morning", rmeq_score >= 18 & rmeq_score <= 21 ~ "Rather morning", rmeq_score >= 12 & rmeq_score <= 17 ~ "Neither type", rmeq_score >= 8 & rmeq_score <= 11 ~ "Rather evening", rmeq_score >= 4 & rmeq_score <= 7 ~ "Definitely evening",
                                TRUE ~ NA_character_)) %>%
  mutate(Source = "rMEQ-Based",
         gap = pred_lasso - time_day,
         res_abs = abs(gap))

# 2. Combine into one dataset ----5.5
gap_combined <- bind_rows(gap_assess, gap_online, gap_rmeq) %>%
  mutate(chrono = factor(chronotype, levels = c("Definitely morning", "Rather morning", "Neither type", "Rather evening", "Definitely evening")))

# ---- 3. Summarise mean and sd : GAP and RES_ABS ----
gap_summary <- gap_combined %>%
  group_by(Source, chronotype) %>%
  summarise(mean_gap = mean(gap, na.rm = TRUE),
            sd_gap = sd(gap, na.rm = TRUE),
            mean_res_abs = mean(res_abs, na.rm = TRUE),
            sd_res_abs = sd(res_abs, na.rm = TRUE),
            n = n())

gap_combined %>% 
  group_by(Source, chronotype) %>%
  summarise(mean_gap = mean(gap, na.rm = TRUE),
                           sd_gap = sd(gap, na.rm = TRUE),
                           mean_res_abs = mean(res_abs, na.rm = TRUE),
                           sd_res_abs = sd(res_abs, na.rm = TRUE),
                           n = n())

# ---- 4. Plot gap
ggplot(gap_summary, aes(x = Source, y = mean_gap, color = chronotype)) +
  geom_jitter(size = 3) +
  geom_errorbar(aes(ymin = mean_gap - sd_gap, ymax = mean_gap + sd_gap), width = 0.2) +
  labs(title = "Mean Circadian Acceleration (gap)",
       x = "Chronotype Source",
       y = "Mean gap (± SD)") +
  theme_minimal()

# ---- 5. Plot RES_ABS 
ggplot(gap_summary, aes(x = Source, y = mean_res_abs, color = chronotype)) +
  geom_jitter(size = 3) +
  geom_errorbar(aes(ymin = mean_res_abs - sd_res_abs, ymax = mean_res_abs + sd_res_abs), width = 0.2) +
  labs(title = "Mean Circadian Dysregulation (res_abs)",
       x = "Chronotype Source",
       y = "Mean res_abs (± SD)") +
  theme_minimal()

gap_combined %>%
  filter(!is.na(chrono)) %>%
  ggplot(aes(x = Source, y = gap, fill = chrono)) + 
  geom_boxplot()

gap_combined %>%
  ggplot(aes(x = rmeq_score, y = gap)) + 
  geom_smooth()

