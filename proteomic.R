# Step 1: Load the proteomic data
proteomic_data <- read.csv("path/to/your/proteomic_file.csv")  # replace with your actual path

# Step 2: Merge with chronotype info from df_clean
merged_data <- proteomic_time_prediction_results %>%
  inner_join(df_clean %>% select(eid, rmeq_chronotype), by = "eid")


# Step 3: Create gap and res_abs columns
merged_data <- merged_data %>%
  mutate(
    gap = pred_lasso - time_day,
    res_abs = abs(gap)
  )

# Step 4: Calculate standard deviations by chronotype
library(dplyr)

summary_stats <- merged_data %>%
  group_by(rmeq_chronotype) %>%
  summarise(
    sd_gap = sd(gap, na.rm = TRUE),
    sd_res_abs = sd(res_abs, na.rm = TRUE),
    n = n()
  )

# Step 5: View the result
print(summary_stats)
