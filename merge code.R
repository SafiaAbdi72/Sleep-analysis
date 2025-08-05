# Load necessary libraries
library(tidyverse)
library(ggpubr)
# load proteomics data

# Sleep Assessment Centre chronotype
sleep_assess_chrono <- sleep_assessment %>%
  mutate(
    chronotype_assess = case_when(
      p1180_i0 == "Definitely a 'morning' person" ~ "Definitely morning",
      p1180_i0 == "More a 'morning' than 'evening' person" ~ "Rather morning",
      p1180_i0 == "More an 'evening' than a 'morning' person" ~ "Rather evening",
      p1180_i0 == "Definitely an 'evening' person" ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, chronotype_assess)

# Sleep Online chronotype
sleep_online_chrono <- sleep_online %>%
  mutate(
    chronotype_online = case_when(
      p30429 == "Definitely a morning-type" ~ "Definitely morning",
      p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning",
      p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening",
      p30429 == "Definitely an evening-type" ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, chronotype_online)

# rMEQ-based chronotype from rmeq_scoring
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


# Step 1: Prepare circadian disruption measures
proteomics <- proteomics %>%
  mutate(
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )

# Step 2: Join proteomics data with chronotype data from all 3 sources
merged_proteom <- proteomics %>%
  left_join(sleep_online_chrono, by = "eid") %>%
  left_join(sleep_assess_chrono, by = "eid") %>%
  left_join(rmeq_scoring %>% select(eid, rmeq_chronotype), by = "eid")

# Step 3: Reshape data to long format for all three sources
long_df <- merged_proteom %>%
  pivot_longer(
    cols = c(chronotype_online, chronotype_assess, rmeq_chronotype),
    names_to = "source",
    values_to = "chronotype"
  ) %>%
  filter(!is.na(chronotype), !is.na(res_abs), !is.na(gap))

