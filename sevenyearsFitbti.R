library(tidyverse)
library(ggplot2)
library(ggthemes)
library(grid)
library(lubridate)
library(here)
library(scales)

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
dailyactivity <- dailyactivity %>% 
  mutate(year = year(ActivityDate),
         day = wday(ActivityDate, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                             day > 5 ~ "Weekend"))
         )

# break up date and time into separate variables for minute files
heartrate_1min <-  heartrate_1min %>% 
  mutate(date = as.Date(Time),
         day = wday(date, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                                       day > 5 ~ "Weekend")),
         timeonly = format(as.POSIXct(strptime(Time, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M"),
         plottime = as.POSIXct(timeonly, format = "%H:%M")
  )

steps_1min <-  steps_1min %>% 
  mutate(date = as.Date(ActivityMinute),
         day = wday(date, label = FALSE, week_start = getOption("lubridate.week.start", 1)),
         weekday = as.factor(case_when(day < 6 ~ "Weekday",
                                       day > 5 ~ "Weekend")),
         timeonly = format(as.POSIXct(strptime(ActivityMinute, "%Y-%m-%d  %H:%M:%S",tz="")) ,format = "%H:%M"),
         plottime = as.POSIXct(timeonly, format = "%H:%M")
  )

# Steps Per Year plot
steps_year_plot <- ggplot(dailyactivity, aes(year, TotalSteps)) +
  geom_col(fill = "#1C3F6F") +
  scale_x_continuous(breaks = 2011:2018) +
  scale_y_continuous(labels = comma, limits = c(0, 5000000)) +
  labs(title = "Steps Per Year",
       subtitle = "26 Million Steps",
       x='Year', 
       y='Fitbit Steps') +
  theme_hc()

steps_year_plot

# minute steps plot
all_minute_steps <- ggplot(steps_1min, aes(x=plottime, y=Steps)) + 
  geom_point(size=.5, alpha = 0.05) + 
  scale_x_datetime(breaks=date_breaks("4 hour"), labels=date_format("%H:%M", tz="")) +
  labs(title='All of the Steps') +
  theme(legend.position="none") +
  theme_hc()

ggsave("allminutesteps.pdf", width = 13.33, height = 7.5, units = "in")

# minute heart rate plot
all_minute_hr <- ggplot(heartrate_1min, aes(plottime, Value)) +
  geom_point(size=.5, alpha = 0.1, color="red") + 
  scale_x_datetime(breaks=date_breaks("4 hour"), labels=date_format("%H:%M", tz="")) +
  labs(title="One Year of Heart Rate Data", 
       subtite="October 5, 2016 - October 4, 2017",
       x = "Time", 
       y = "Heart Rate") 


ggsave("allminutehr.tiff", width = 13.33, height = 7.5, units = "in")



