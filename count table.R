
# 1. Prepare Sleep Online: select relevant columns and remove blanks
sleep_online_chrono <- sleep_online %>%
  mutate(p30429 = na_if(p30429, "")) %>%
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

# 2. Prepare Sleep Assessment Centre: select relevant columns and remove blanks
sleep_assess_chrono <- sleep_assessment %>%
  mutate(p1180_i2 = na_if(p1180_i2, "")) %>%
  mutate(
    chronotype_assess = case_when(
      p1180_i2 == "Definitely a 'morning' person" ~ "Definitely morning",
      p1180_i2 == "More a 'morning' than 'evening' person" ~ "Rather morning",
      p1180_i2 == "More an 'evening' than a 'morning' person" ~ "Rather evening",
      p1180_i2 == "Definitely an 'evening' person" ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  ) %>%
  select(eid, chronotype_assess)

# 3. Merge datasets by eid (keep only those with both responses)
merged_chrono <- inner_join(sleep_online_chrono, sleep_assess_chrono, by = "eid")

# 4. Create a cross-tabulated count table with labelled axes
chronotype_table <- table(
  `Sleep Online` = merged_chrono$chronotype_online,
  `Sleep Assessment Centre` = merged_chrono$chronotype_assess
)

# 5. Optional: Convert to a data frame for display or export
chronotype_df <- as.data.frame.matrix(chronotype_table)

# 6. View result
print(chronotype_table)

chronotype_df <- as.data.frame.matrix(chronotype_table)
