library(dplyr)
library(ggplot2)
library(ggalluvial)

# Step 1: Chronotype categorisation from all sources (same as before)
# Sleep Assessment Centre
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

# Sleep Online
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
# Make sure rmeq_score exists
df <- df %>%
  mutate(
    rmeq_score = score_wake_up + score_tired_after_waking +
      score_evening_tiredness + score_best_time + score_chronotype
  )


# rMEQ-Based Chronotype
df <- df %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 & rmeq_score <= 25 ~ "Definitely morning",
      rmeq_score >= 18 & rmeq_score <= 21 ~ "Rather morning",
      rmeq_score >= 12 & rmeq_score <= 17 ~ "Neither type",
      rmeq_score >= 8  & rmeq_score <= 11 ~ "Rather evening",
      rmeq_score >= 4  & rmeq_score <= 7  ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, rmeq_chronotype)

# Step 2: Merge all chronotype sources
all_chrono <- sleep_assess_chrono %>%
  inner_join(sleep_online_chrono, by = "eid") %>%
  inner_join(df, by = "eid")

# Step 3: Drop incomplete rows and set factor order
all_chrono_clean <- all_chrono %>%
  filter(
    !is.na(chronotype_assess),
    !is.na(chronotype_online),
    !is.na(rmeq_chronotype)
  ) %>%
  mutate(
    chronotype_assess = factor(chronotype_assess,
                               levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    chronotype_online = factor(chronotype_online,
                               levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    rmeq_chronotype = factor(rmeq_chronotype,
                             levels = c("Definitely morning", "Rather morning", "Neither type", "Rather evening", "Definitely evening"))
  )

# Step 4: Calculate proportions instead of counts
total_n <- nrow(all_chrono_clean)

alluvial_data <- all_chrono_clean %>%
  count(chronotype_assess, chronotype_online, rmeq_chronotype) %>%
  mutate(proportion = n / total_n)

# Step 5: Plot the alluvial diagram by proportion
ggplot(alluvial_data,
       aes(axis1 = chronotype_assess,
           axis2 = chronotype_online,
           axis3 = rmeq_chronotype,
           y = proportion)) +
  geom_alluvium(aes(fill = chronotype_assess), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Assessment Centre", "Sleep Online", "rMEQ-Based"),
                   expand = c(.05, .05)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Chronotype Transitions Across All Sources (Proportion)",
    y = "Proportion of Participants"
  ) +
  theme_minimal()
---------------------------------

library(dplyr)
library(ggalluvial)
library(ggplot2)

# 1. Prepare chronotype from Sleep Assessment Centre
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

# 2. Prepare chronotype from Sleep Online
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

# 3. Prepare rMEQ-Based Chronotype from df_clean
df_clean <- df_clean %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Neither type",
      rmeq_score >= 8  ~ "Rather evening",
      rmeq_score >= 4  ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, rmeq_chronotype)

# 4. Merge all sources
all_chrono <- sleep_assess_chrono %>%
  left_join(sleep_online_chrono, by = "eid") %>%
  left_join(df_clean, by = "eid")

# 6. Count and calculate proportions
all_chrono_c <- all_chrono %>%
  count(chronotype_assess, chronotype_online, rmeq_chronotype) %>%
  mutate(proportion = n / sum(n))


# 5. Clean and order categories
all_chrono_f<- all_chrono_c %>%
  filter(
    !is.na(chronotype_assess),
    !is.na(chronotype_online),
    !is.na(rmeq_chronotype)
  ) %>%
  mutate(
    chronotype_assess = factor(chronotype_assess,
                               levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    chronotype_online = factor(chronotype_online,
                               levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    rmeq_chronotype = factor(rmeq_chronotype,
                             levels = c("Definitely morning", "Rather morning", "Neither type", "Rather evening", "Definitely evening"))
  )

# 7. Alluvial plot with proportions

ggplot(all_chrono_f,
       aes(axis1 = chronotype_assess,
           axis2 = chronotype_online,
           axis3 = rmeq_chronotype,
           y = n)) +
  geom_alluvium(aes(fill = chronotype_assess), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Assessment Centre", "Sleep Online", "rMEQ-Based"),
                   expand = c(.05, .05)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportional Alluvial Plot: Chronotype Transitions",
    y = "Proportion of Participants"
  ) +
  theme_minimal()


