# Load libraries
install.packages("ggalluvial")
library(ggplot2)
library(dplyr)
library(ggalluvial)

# Step 1: Prepare each chronotype dataset

## Sleep Assessment Centre
sleep_assess_chrono <- sleep_assessment %>%
  select(eid, p1180_i0) %>%
  filter(p1180_i0 != "") %>%
  mutate(chronotype_assess = case_when(
    p1180_i0 == "Definitely a 'morning' person" ~ "Definitely morning",
    p1180_i0 == "More a 'morning' than 'evening' person" ~ "Rather morning",
    p1180_i0 == "More an 'evening' than a 'morning' person" ~ "Rather evening",
    p1180_i0 == "Definitely an 'evening' person" ~ "Definitely evening",
    TRUE ~ NA_character_
  )) %>%
  select(eid, chronotype_assess)

## Sleep Online
sleep_online_chrono <- sleep_online %>%
  select(eid, p30429) %>%
  filter(p30429 != "") %>%
  mutate(chronotype_online = case_when(
    p30429 == "Definitely a morning-type" ~ "Definitely morning",
    p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning",
    p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening",
    p30429 == "Definitely an evening-type" ~ "Definitely evening",
    TRUE ~ NA_character_
  )) %>%
  select(eid, chronotype_online)

## Self-reported and rMEQ-based from df
self_rmeq_df <- df %>%
  filter(!is.na(p30429), !is.na(rmeq_score)) %>%
  mutate(
    self_reported_chronotype = case_when(
      p30429 == "Definitely a morning-type" ~ "Definitely morning",
      p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning",
      p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening",
      p30429 == "Definitely an evening-type" ~ "Definitely evening",
      TRUE ~ NA_character_
    ),
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Neither type",
      rmeq_score >= 8  ~ "Rather evening",
      rmeq_score < 8   ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, self_reported_chronotype, rmeq_chronotype)

# Step 2: Merge all chronotype datasets
alluvial_data <- sleep_assess_chrono %>%
  inner_join(sleep_online_chrono, by = "eid") %>%
  inner_join(self_rmeq_df, by = "eid") %>%
  filter(!is.na(chronotype_assess), !is.na(chronotype_online),
         !is.na(self_reported_chronotype), !is.na(rmeq_chronotype))

# Step 3: Prepare alluvial data
alluvial_counts <- alluvial_data %>%
  count(
    chronotype_assess,
    chronotype_online,
    self_reported_chronotype,
    rmeq_chronotype
  )

# Step 4: Plot alluvial diagram
alluvial_plot <- ggplot(alluvial_counts,
                        aes(axis1 = chronotype_assess,
                            axis2 = chronotype_online,
                            axis3 = self_reported_chronotype,
                            axis4 = rmeq_chronotype,
                            y = n)) +
  geom_alluvium(aes(fill = chronotype_assess), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Assessment Centre", "Sleep Online", "Self-Reported", "rMEQ-Based"),
                   expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  labs(
    title = "Chronotype Flow Across Assessments",
    y = "Number of Participants"
  ) +
  theme_minimal()

# Print the plot
print(alluvial_plot)
--------------------------------------------# newest one I think?
  # Install necessary package
  install.packages("ggalluvial")
library(ggalluvial)
library(dplyr)
library(ggplot2)

# 1. Prepare chronotype data from all four stages
# Standardise wording and merge chronotype sources

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

# Self-Reported (same as sleep online, just renamed for clarity)
self_reported_chrono <- sleep_online_chrono %>%
  rename(self_reported_chronotype = chronotype_online)

# rMEQ-Based Chronotype from df
df <- df %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Neither type",
      rmeq_score >= 8  ~ "Rather evening",
      rmeq_score < 8   ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, rmeq_chronotype)

# Merge all chronotype sources by eid
all_chrono <- sleep_assess_chrono %>%
  inner_join(sleep_online_chrono, by = "eid") %>%
  inner_join(self_reported_chrono, by = "eid") %>%
  inner_join(df, by = "eid")

# Drop rows with any missing chronotype
all_chrono_clean <- all_chrono %>%
  filter(
    !is.na(chronotype_assess),
    !is.na(chronotype_online),
    !is.na(self_reported_chronotype),
    !is.na(rmeq_chronotype)
  )

# Set factor levels to desired order
all_chrono_clean <- all_chrono_clean %>%
  mutate(
    chronotype_assess = factor(chronotype_assess, levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    chronotype_online = factor(chronotype_online, levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    self_reported_chronotype = factor(self_reported_chronotype, levels = c("Definitely morning", "Rather morning", "Rather evening", "Definitely evening")),
    rmeq_chronotype = factor(rmeq_chronotype, levels = c("Definitely morning", "Rather morning", "Neither type", "Rather evening", "Definitely evening"))
  )

# Count transitions for plotting
alluvial_data <- all_chrono_clean %>%
  count(chronotype_assess, chronotype_online, self_reported_chronotype, rmeq_chronotype)



# Plot the alluvial diagram
ggplot(alluvial_data,
       aes(axis1 = chronotype_assess,
           axis2 = chronotype_online,
           axis3 = self_reported_chronotype,
           axis4 = rmeq_chronotype,
           y = n)) +
  geom_alluvium(aes(fill = chronotype_assess), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey90", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("Assessment Centre", "Sleep Online", "Self-Reported", "rMEQ-Based"),
                   expand = c(.05, .05)) +
  labs(
    title = "Chronotype Transitions Across All Sources",
    y = "Number of Participants"
  ) +
  theme_minimal()

