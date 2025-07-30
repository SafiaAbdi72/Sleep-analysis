# Comparing online to online: self-reported chrontype to computed chronotype nased on rMEQ score
# assign computed chronotype based on score 
df <- df %>%
  mutate(
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Intermediate",
      rmeq_score >= 8  ~ "Rather evening",
      rmeq_score >= 4  ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  )

# extracting self-reported chronotype and standardise wording 
df <- df %>%
  mutate(
    self_reported_chronotype = case_when(
      p30429 == "Definitely a morning-type" ~ "Definitely morning",
      p30429 == "Rather more a morning-type than an evening-type" ~ "Rather morning",
      p30429 == "Rather more an evening-type than a morning-type" ~ "Rather evening",
      p30429 == "Definitely an evening-type" ~ "Definitely evening",
      TRUE ~ NA_character_
    )
  )
# creating cross-tabulation 
# Filter only valid rows
compare_df <- df %>%
  filter(!is.na(self_reported_chronotype), !is.na(rmeq_chronotype))

# Create the table
consistency_table <- table(
  `Self-Reported` = compare_df$self_reported_chronotype,
  `rMEQ-Based` = compare_df$rmeq_chronotype
)

# Optional: Turn into data frame
consistency_df <- as.data.frame.matrix(consistency_table)
--------------------------------------------------------------------------------
  df <- df %>%
  mutate(
    # Based on rMEQ score
    rmeq_chronotype = case_when(
      rmeq_score >= 22 ~ "Definitely morning",
      rmeq_score >= 18 ~ "Rather morning",
      rmeq_score >= 12 ~ "Rather evening",
      rmeq_score < 12  ~ "Definitely evening",
      TRUE ~ NA_character_
    ),
    
    # Self-reported chronotype from p30429
    self_reported_chronotype = case_when(
      p30429 == "Definitely a morning-type" ~ "Definitely morning-type",
      p30429 == "Rather more a morning-type than an evening-type" ~ "Rather more a morning-type than an evening-type",
      p30429 == "Rather more an evening-type than a morning-type" ~ "Rather more an evening-type than a morning-type",
      p30429 == "Definitely an evening-type" ~ "Definitely an evening-type",
      TRUE ~ NA_character_
    )
  )

# View table
print(consistency_table)

ggplot(df_comp, aes(x = rmeq_chronotype, fill = self_reported_chronotype)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Comparison of Self-Reported vs rMEQ-Derived Chronotypes",
    x = "rMEQ Chronotype",
    y = "Count",
    fill = "Self-Reported"
  ) +
  theme_minimal()
# create filtered dataset
df_comp <- df %>%
  filter(!is.na(rmeq_chronotype), !is.na(self_reported_chronotype))
# plots - heatmap? *not sure
chronotype_matrix <- table(df_comp$self_reported_chronotype, df_comp$rmeq_chronotype)
chronotype_df <- as.data.frame(chronotype_matrix)

ggplot(chronotype_df, aes(x = Var2, y = Var1, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Self-Reported vs rMEQ-Based Chronotype",
    x = "rMEQ Chronotype",
    y = "Self-Reported Chronotype",
    fill = "Count"
  ) +
  theme_minimal()
# bar plot 
ggplot(df_comp, aes(x = self_reported_chronotype, fill = rmeq_chronotype)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Comparison of Self-Reported and rMEQ Chronotypes",
    x = "Self-Reported Chronotype",
    y = "Count",
    fill = "rMEQ Chronotype"
  ) +
  theme_minimal()
# new plot - alluvial
install.packages("ggalluvial")
library(ggalluvial)
alluvial_df <- df_comp %>%
  count(self_reported_chronotype, rmeq_chronotype) %>%
  filter(!is.na(self_reported_chronotype) & !is.na(rmeq_chronotype))
ggplot(alluvial_df,
       aes(axis1 = self_reported_chronotype,
           axis2 = rmeq_chronotype,
           y = n)) +
  geom_alluvium(aes(fill = self_reported_chronotype), width = 1/12) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Self-Reported", "rMEQ-Based"), expand = c(.1, .1)) +
  labs(title = "Alluvial Plot: Self-Reported vs rMEQ Chronotype",
       y = "Number of Participants") +
  theme_minimal()
