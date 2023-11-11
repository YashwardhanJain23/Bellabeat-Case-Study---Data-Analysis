# **Table of contents**

# [1. Introduction]
# [2. Ask Phase]
# [3. Prepare Phase]
# [4. Process Phase]
# [5. Analyze and Share Phase]
# [6. Act Phase]


# 1. About the Company
# Bellabeat is a high-tech business that creates smart products with a focus on health that was founded by Urška Sršen and Sando Mur.
# Sršen used her training as an artist to create elegantly crafted technology that educates and empowers women all over the world. With the help of data collection on activity, sleep, stress, and reproductive health, Bellabeat has been able to educate women about their own habits and health. Bellabeat has rapidly expanded since its founding in 2013 and established itself as a tech-driven wellness company for women.

# 2. Ask
# We need to establish the kind of questions we need to ask ourselves before starting the analysis. What is the problem you are trying to solve? Who are the stakeholders? What insights could help with the business decisions?
# The business task is:
# Determine consumer behaviour trends for smart devices other than Bellabeat to assist them in developing a marketing plan.
# The main stakeholders are:
# 1) Urška Sršen: Bellabeat’s cofounder and Chief Creative Officer
# 2) Sando Mur: Mathematician and Bellabeat’s cofounder; key member of the Bellabeat executive team
# 3) Bellabeat marketing analytics team


# 3. Prepare
# About the Data
# The "FitBit Fitness Tracker" dataset, which will be used for this analysis, was retrieved from Kaggle. Thirty Fitbit users' personal fitness trackers are included in this Kaggle data set. Thirty eligible Fitbit users agreed to submit their personal tracker data, which included minute-level output for heart rate, sleep, and physical activity monitoring. It provides data on steps, heart rate, and daily activity that can be used to examine users' routines.
# Organization of Data
# The dataset consists of 18.csv files. There are 15 in long format and 3 in wide format. The datasets include data on steps, heart rate, metabolic equivalent of tasks (METs), calories, and activity metrics over timescales of seconds, minutes, hours, and days.
# Data Limitations: 
# 1) It has a small sample size. Only 30 users have been used to determine the whole population of Bellabeat consumers, which can result into bias
# 2) Demographic information is not provided which again causes bias.


# 4. Process
# I will be using R programming for the process phase. The dataset will be cleaned and formatted to be used for the analysis.

# Installing R Packages
# The packages I will be installing and be using for this analysis are:
# tidyverse
# readr
# skimr
# lubridate
# janitor
# dplyr

library(tidyverse)
library(readr)
library(skimr)
library(lubridate)
library(janitor)
library(dplyr)

# Importing Datasets
# The datasets are I will be using for this analysis are the following:

# dailyActivity_merged
# hourlyCalories_merged
# hourlyIntensities_merged
# hourlySteps_merged
# heartrate_seconds_merged
# sleepDay_merged

dailyActivity <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
heartrate_seconds <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
hourlyCalories <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourlyIntensities <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourlySteps <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
sleepDay <- read_csv("FitBit Fitness Tracker Data Set/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")

#Viewing the Datasets
# Using head(), I will viewing the first 6 columns of each dataset I have chosen

head(dailyActivity)
head(heartrate_seconds)
head(hourlyCalories)
head(hourlyIntensities)
head(hourlySteps)
head(sleepDay)

# Cleaning and Formatting the Datasets

clean_names(dailyActivity)
clean_names(heartrate_seconds)
clean_names(hourlyCalories)
clean_names(hourlyIntensities)
clean_names(hourlySteps)
clean_names(sleepDay)

# Checking for any duplicate values

sum(duplicated(dailyActivity))
sum(duplicated(heartrate_seconds))
sum(duplicated(hourlyCalories))
sum(duplicated(hourlyIntensities))
sum(duplicated(hourlySteps))
sum(duplicated(sleepDay))

# Through this, we get that there 3 duplicates in the sleepDay dataset. Now removing and verifying the duplicates

sleepDay <- sleepDay %>%
  distinct()
sum(duplicated(sleepDay))

# 5. Analyze and Share
# Summarizing Data
# We will review the summary of all the datasets. It is as follows

summary(dailyActivity)
summary(hourlyCalories)
summary(hourlyIntensities)
summary(hourlySteps)
summary(heartrate_seconds)
summary(sleepDay)

# Findings

# 1) There are more average sedentary minutes (991) than average lightly active minutes (192.8), fairly active minutes (13.56), very active minutes (21.16)
# 2) The average calories burnt every hour is 97.39
# 3) There are an average of 320 steps taken every hour
# 4) The maximum heart rate of a user is 203
# 5) The max amount of time spent in bed (961) is more than time spent asleep (796). On average, a user spends 419 minutes asleep which is aorund 7 hours.

# Now, we will find the number of users

n_distinct(dailyActivity)
n_distinct(heartrate_seconds)
n_distinct(sleepDay)

# Now, we will find out at what time are users most active

hourlyIntensities$ActivityHour <- parse_date_time(hourlyIntensities$ActivityHour, "%d/%m/%y %I:%M:%S %p")
hourlyIntensities$Date <- as.Date(hourlyIntensities$ActivityHour, format = "%d/%m/%y")
hourlyIntensities$Time <- format(hourlyIntensities$ActivityHour, format = "%H:%M:%S")


ggplot(data=hourlyIntensities,aes(x=Time,y=TotalIntensity)) + geom_histogram(stat = "identity", fill="red") + theme(axis.text.x = element_text(angle = 90)) + labs(title = 'Total Intensity vs Time')

#Due to certain errors, as mentioned in the message, the plot was not made accurately but from the plot, it was seen that the users were most active during the hours of 12:00 to 14:00 and 16:00 to 20:00

# Finding Correlations
# Total Time Spent Asleep vs Total Time Spent in Bed

ggplot(data = sleepDay, aes(x= TotalMinutesAsleep, y = TotalTimeInBed))+ geom_point() + geom_smooth() + labs(title = "Total Time Spent Asleep vs Total Time Spent In Bed.")

# As seen this has a positive correlation, which means people are spending more time in their bed and doing miscellaneous activities, rather than sleeping.

# Total Steps vs Calories Burned

ggplot(dailyActivity,aes(x = TotalSteps, y = Calories))+ geom_point () + geom_smooth() + labs(title = "Total Steps vs Calories.")

# As expected, there is a positive correlation between calories burnt and total steps, according to the above graph.

# 6. Act
# Suggestions

# These are recommendations I would give to the marketing team to which they can accordingly change their marketing strategy.
# Users should be able to enter and interact with their personal health data using an interactive feature in the Bellabeat app.
# For our users, the daily average for steps was 7638. Users who surpass their average step count could receive rewards, as taking more steps each day is linked to better health.
# When heart rates reach an extreme level, the app should introduce a notification warning.
# Users sleep for a little under 7 hours on average. The app should have a feature that alerts users to sleep so they can get the recommended amount of sleep and set alarms accordingly to wake them up after averaging a typical amount of sleep.
