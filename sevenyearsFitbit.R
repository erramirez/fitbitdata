library(tidyverse)
library(ggplot2)
library(ggthemes)
library(grid)
library(lubridate)
library(here)
library(scales)
library(hrbrthemes)

import_roboto_condensed()

##### Read in Data #####

# daily activity file read in with date format change
dailyactivity <- read_csv(here("data", "dailyActivity.csv"), 
                          col_types = cols(ActivityDate = col_date(format = "%m/%d/%Y"))
                          )

# activity log file read in with date and time format change
activitylogs <- read_csv(here("data", "activitylogs.csv"), 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y"),
                                           StartTime = col_time(format = "%H:%M:%S")
                                           )
                         )

# heart rate file read in with datetime format change
heartrate_1min <- read_csv(here("data", "heartrate_1min.csv"), 
                           col_types = cols(Time = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"))
                           )

# activity intensity minute classification file read in with datetime format change
intensity_1min <- read_csv(here("data", "minuteIntensitiesNarrow.csv"), 
                           col_types = cols(ActivityMinute = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"))
                           )

# steps per minute file read in with datetime format change
steps_1min <- read_csv(here("data", "minuteStepsNarrow.csv"), 
                           col_types = cols(ActivityMinute = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"))
                       )


##### Add Additional Variables #####

# add year to daily activity
# also add weekday variables
# use stepday to set days with > 0 steps 
dailyactivity <- dailyactivity %>% 
  mutate(year = year(ActivityDate),
         month = month(ActivityDate),
         day = wday(ActivityDate, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                             day > 5 ~ "Weekend")),
         stepday = ifelse(TotalSteps > 0, 1, 0) 
         )

# break up date and time into separate variables for minute files
# use getOption("lubridate.week.start", 1) to set week to start on Monday
# plottime appends current date for POSIXct format - useful during plotting
heartrate_1min <-  heartrate_1min %>% 
  mutate(date = as.Date(Time),
         year = year(date),
         month = month(date),
         day = wday(date, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                                       day > 5 ~ "Weekend")),
         timeonly = format(as.POSIXct(strptime(Time, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M"),
         plottime = as.POSIXct(timeonly, format = "%H:%M")
  )

# break up date and time into separate variables for minute files
# use getOption("lubridate.week.start", 1) to set week to start on Monday
# plottime appends current date for POSIXct format - useful during plotting
steps_1min <-  steps_1min %>% 
  mutate(date = as.Date(ActivityMinute),
         year = year(date),
         month = month(date),
         day = wday(date, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                                       day > 5 ~ "Weekend")),
         timeonly = format(as.POSIXct(strptime(ActivityMinute, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M"),
         plottime = as.POSIXct(timeonly, format = "%H:%M")
  )

# append stepday classification to minute step data
steps_1min_stepdaysonly <- dailyactivity %>% 
  select(ActivityDate, stepday) %>% 
  rename(., date = ActivityDate) %>% # rename ActivityDate to date for easy merge
  left_join(steps_1min, .)

##### Data Aggregation #####

# calculate number of "step days" per year
stepdays <- dailyactivity %>% 
  group_by(year) %>% 
  summarise(stepdays = sum(stepday),
            totalsteps = sum(TotalSteps))

# calculate mean and sd heart rate for weekdays and weekend days
meanhr <- heartrate_1min %>% 
  group_by(timeonly, weekday) %>% 
  summarise(mean = mean(Value),
            sd = sd(Value)
            ) %>% 
  mutate(plottime = as.POSIXct(timeonly, format = "%H:%M"))

# create pre/post baby data set
babydata_dailyactivity <- dailyactivity %>% 
  filter(ActivityDate >= "2017-11-26" & ActivityDate <= "2018-04-15")

babydata_steps1min<- steps_1min %>% 
  filter(date >= "2017-11-26" & date <= "2018-04-15")


##### Plots #####

# Steps Per Year plot
steps_year_plot <- ggplot(dailyactivity, aes(year, TotalSteps)) +
  geom_col(fill = "#1C3F6F") +
  geom_text(data = stepdays, aes(x = year, y = (totalsteps - 150000), label= stepdays
                ),
            hjust = .5, 
            size = 6,
            color = "white") +
  scale_x_continuous(breaks = 2011:2018) +
  scale_y_continuous(labels = comma, limits = c(0, 5000000)) +
  labs(title = "26 Million Fitbit Steps",
       subtitle = "Feb 27, 2011 - April 20, 2018",
       x = "Year", 
       y = "Fitbit Steps",
       caption = "Bar labels indicate total number of wear days with >0 steps.") +
  theme_ipsum_rc()

ggsave("stepsyear.tiff", width = 13.33, height = 7.5, units = "in")

# minute steps plot
all_minute_steps <- ggplot(steps_1min_stepdaysonly, aes(x = plottime, y = Steps)) + 
  geom_point(size = .5, alpha = 0.04, colour = "#1C3F6F") + 
  scale_x_datetime(breaks=date_breaks("4 hour"), labels = date_format("%H:%M", tz = "")) +
  labs(title = "Steps per Minute",
       subtitle = "Visualizing 28M Steps Across 7+ Years",
       x = "Time of Day",
       y = "Step Count") +
  theme(legend.position="none") +
  theme_ipsum_rc()

ggsave("allminutesteps.tiff", width = 13.33, height = 7.5, units = "in")

# minute heart rate plot
all_minute_hr <- ggplot(heartrate_1min, aes(plottime, Value)) +
  geom_point(size = .5, alpha = 0.05, color="red") + 
  scale_x_datetime(breaks=date_breaks("4 hour"), labels = date_format("%H:%M", tz = "")) +
  labs(title = "775,549 Heart Rates", 
       subtitle = "October 5, 2016 - April 20, 2018",
       x = "Time of Day", 
       y = "Heart Rate (beats per minute)") +
  theme_ipsum_rc()

ggsave("allminutehr.tiff", width = 13.33, height = 7.5, units = "in")

# minute heart rate plot + weekday average
weekday_mean_hr <- ggplot(heartrate_1min, aes(plottime, Value)) +
  geom_point(size = .5, alpha = 0.05, color="grey") + 
  geom_point(data = subset(meanhr, weekday == "Weekday"), aes(plottime, mean), 
             size = .5, color="red") +
  scale_x_datetime(breaks=date_breaks("4 hour"), labels = date_format("%H:%M", tz = "")) +
  labs(title = "Weekday Mean Heart Rate ", 
       subtitle = "October 5, 2016 - April 20, 2018",
       x = "Time of Day", 
       y = "Heart Rate (beats per minute)") +
  theme_ipsum_rc()

ggsave("weekdaymeanhr.tiff", width = 13.33, height = 7.5, units = "in")

# minute heart rate plot + weekend average
weekend_mean_hr <- ggplot(heartrate_1min, aes(plottime, Value)) +
  geom_point(size = .5, alpha = 0.05, color="grey") + 
  geom_point(data = subset(meanhr, weekday == "Weekend"), aes(plottime, mean), 
             size = .5, color = "yellow") +
scale_x_datetime(breaks=date_breaks("4 hour"), labels=date_format("%H:%M", tz = "")) +
  labs(title = "Weekend Mean Heart Rate ", 
       subtitle = "October 5, 2016 - April 20, 2018",
       x = "Time of Day", 
       y = "Heart Rate (beats per minute)") +
  theme_ipsum_rc()

ggsave("weekendmeanhr.tiff", width = 13.33, height = 7.5, units = "in")


