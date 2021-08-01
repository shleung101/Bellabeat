library(tidyverse)
library(lubridate)

# rename df
activity <- dailyActivity_merged
calories <- dailyCalories_merged

# separate date time into data and time columns, drop time column
sleep <- sleepDay_merged %>% 
  separate(SleepDay, c("SleepDay", "other"), sep = " ") %>% select(-other)



typeof(activity$ActivityDate)

dim(activity)

# Join activity and calories df
act_cal <- left_join(activity, calories, by = c("Id", "ActivityDate" = "ActivityDay", "Calories"))

count(act_cal)


# Join act_cal df with sleep df, dropped 527 entries with NAs, removed 3 duplicates
fitbit_data <- left_join(act_cal, sleep, by = c("Id", "ActivityDate" = "SleepDay")) %>% 
  na.omit() %>% 
  unique()

dim(fitbit_data)

# Check reasonable ranges 

# Sedentary Active Distance should be close to 0 because you're not moving
fitbit_data %>% summarise(min_sedentary = min(SedentaryActiveDistance), max_sedentary = max(SedentaryActiveDistance))


# Check if all distances add up 
a <- fitbit_data %>% 
  mutate(temp_total = TrackerDistance + LoggedActivitiesDistance + SedentaryActiveDistance + LightActiveDistance + ModeratelyActiveDistance + VeryActiveDistance) %>%
  select(Id, TotalDistance, temp_total)%>% 
  mutate(difference = TotalDistance - temp_total) %>%  # take difference to compare if values are close enough
  filter(difference > 0.1)                             # filter if 
  

### Analysis ### 

# ASSUMPTION: time to fall asleep corresponds to the same day as the total activity done in the same row 

fitbit_data %>% group_by(Id) %>% 
  summarise(avg_time_fall_asleep = mean(TotalMinutesFallAsleep), 
            avg_steps = mean(TotalSteps), 
            avg_distance = mean(TotalDistance)) %>% 
  arrange(-avg_steps) %>% 
  print(n=40)

# average time to fall asleep is between 10 - 20 minutes
mean(fitbit_data$TotalMinutesFallAsleep)

# average steps taken a day = 3000 - 4000
mean(fitbit_data$TotalSteps)
min(fitbit_data$TotalSteps)
max(fitbit_data$TotalSteps)


# Average min of sleep per night should be 7-9 hrs
mean(fitbit_data$TotalMinutesAsleep)/60
min(fitbit_data$TotalMinutesAsleep)/60
max(fitbit_data$TotalMinutesAsleep)/60

# Goal: compare if doing more exercise = sleep better and less exercise = sleep worse

# Create category for ppl to fall asleep 
# Slow: 20
# Normal: 10 - 20
# Fast: < 10  Note: fast sleeper may indicate sleep duration too short




fitbit_data %>% 
  mutate(fall_asleep_cat = case_when(TotalMinutesFallAsleep < 10 ~ "Fast",
                                                   TotalMinutesFallAsleep >= 10 & TotalMinutesFallAsleep <= 20 ~ "Normal",
                                                   TotalMinutesFallAsleep > 20 & TotalMinutesFallAsleep <= 60~ "Slow", 
                                                   TotalMinutesFallAsleep > 60 ~ "Very Slow") ) %>% 
  group_by(fall_asleep_cat) %>%
  summarise(count = n(), avg_steps = mean(TotalSteps), 
            avg_seden_min = mean(SedentaryMinutes), 
            avg_sleep_dur = mean(TotalMinutesAsleep),
            active_min = mean(VeryActiveMinutes),
            avg_cal_burned = mean(Calories)) 

# Conclusion: 

# FAST: People who fall asleep more quickly, sleep less than 5 hrs, and have more sedentary minutes 
### maybe don't feel like exercising bc too tired, fall asleep quickly bc already sleep deprived, actually burn the most calories 
### avg_seden_min higher mebbe bc they're awake for more hours of the day

# VERY SLOW: burn the least calories, spend the least time working out
  
clean <- fitbit_data %>% 
  mutate(fall_asleep_cat = case_when(TotalMinutesFallAsleep < 10 ~ "Fast",
                                     TotalMinutesFallAsleep >= 10 & TotalMinutesFallAsleep <= 20 ~ "Normal",
                                     TotalMinutesFallAsleep > 20 & TotalMinutesFallAsleep <= 60~ "Slow", 
                                     TotalMinutesFallAsleep > 60 ~ "Very Slow") )

