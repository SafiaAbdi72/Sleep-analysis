hi

# install necessary packages
install.packages("tidyverse")
install.packages("ggpubr")
library(tidyverse)
library(ggpubr)
# load 3 datasets 
covariates <- data.table::fread("/mnt/project/clara/covariates.csv")
sleep_assessment <- data.table::fread("/mnt/project/Safia/Sleep_Assessment_Centre_All.csv")
sleep_online <- data.table::fread("/mnt/project/Safia/Sleep_Online_All.csv")
# view datasets 
view(sleep_assessment)
view(sleep_online)
view(covariates)
# Merge age and sex into sleep_online
df <- sleep_online %>%
  left_join(
    covariates %>% select(eid, p31, p21022),
    by = "eid"
  ) %>%
  rename(
    sex = p31,
    age = p21022
  )
#check if merge was succesful 
names(sleep_online)
# Convert empty strings to NA 
df <- df %>%
  mutate(across(where(is.character), ~ na_if(., "")))
# rmeq scoring
df <- df %>%
  mutate(
    score_wake_up = case_when(
      p30425 == "5:00am - 6:30am" ~ 5,
      p30425 == "6:30am - 7:45am" ~ 4,
      p30425 == "7:45am - 9:45am" ~ 3,
      p30425 == "9:45am - 11:00am" ~ 2,
      p30425 == "11:00am - 12 noon" ~ 1,
      TRUE ~ NA_real_
    ),
    
    score_tired_after_waking = case_when(
      p30426 == "Very tired" ~ 1,
      p30426 == "Fairly tired" ~ 2,
      p30426 == "Fairly refreshed" ~ 3,
      p30426 == "Very refreshed" ~ 4,
      TRUE ~ NA_real_
    ),
    
    score_evening_tiredness = case_when(
      p30427 == "8:00pm - 9:00pm" ~ 5,
      p30427 == "9:00pm - 10:15pm" ~ 4,
      p30427 == "10:15pm - 12:45am" ~ 3,
      p30427 == "12:45am - 2:00am" ~ 2,
      p30427 == "2:00am - 3:00am" ~ 1,
      TRUE ~ NA_real_
    ),
    
    score_best_time = case_when(
      p30428 == "5:00am - 8:00am" ~ 5,
      p30428 == "8:00am - 10:00am" ~ 4,
      p30428 == "10:00am - 5:00pm" ~ 3,
      p30428 == "5:00pm - 10:00pm" ~ 2,
      p30428 == "10:00pm - 5:00am" ~ 1,
      TRUE ~ NA_real_
    ),
    
    score_chronotype = case_when(
      p30429 == "Definitely a morning-type" ~ 6,
      p30429 == "Rather more a morning-type than an evening-type" ~ 4,
      p30429 == "Rather more an evening-type than a morning-type" ~ 2,
      p30429 == "Definitely an evening-type" ~ 0,
      TRUE ~ NA_real_
    ),
  )
# only calculate rmeq score if all 5 scores are present 
df <- df %>%
  mutate(
    rmeq_score = if_else(
      !is.na(score_wake_up) &
        !is.na(score_tired_after_waking) &
        !is.na(score_evening_tiredness) &
        !is.na(score_best_time) &
        !is.na(score_chronotype),
      
      score_wake_up +
        score_tired_after_waking +
        score_evening_tiredness +
        score_best_time +
        score_chronotype,
      
      NA_real_
    )
  )
# check rmeq scoring worked 
summary(df$rmeq_score)
# check column names 
names(sleep_online)
# filter out empty rows
df_clean <- df %>%
  filter(
    !is.na(age),
    !is.na(rmeq_score),
    !is.na(sex)
  )

# plot rmeq scores 
ggplot(df_clean, aes(x = age, y = rmeq_score, color = sex)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(
    title = "rMEQ Score vs Age by Sex",
    x = "Age at Assessment",
    y = "rMEQ Score"
  )
# boxplot of rmeq score by sex 
ggplot(df_clean, aes(x = sex, y = rmeq_score, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of rMEQ Scores by Sex",
       x = "Sex",
       y = "rMEQ Score")
# density plot
ggplot(df_clean, aes(x = rmeq_score, color = sex, fill = sex)) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Density Plot of rMEQ Scores by Sex",
       x = "rMEQ Score",
       y = "Density")



