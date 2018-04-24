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
                          col_types = cols(ActivityDate = col_date(format = "%m/%d/%Y"),
                                           Floors = col_integer(),
                                           CaloriesBMR = col_integer(),
                                           MarginalCalories = col_integer(),
                                           RestingHeartRate = col_integer()
                                           ) 
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

sleeplogs <- read_csv(here("data", "sleepStageLogInfo.csv"),
                      col_types = cols(StartTime = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"))
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

# add end datetime for sleep record
sleeplogs <- sleeplogs%>% 
  mutate(duration_period = as.period(Duration/1000, "seconds"),
         EndTime = (StartTime + duration_period)
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
  filter(ActivityDate >= "2017-11-27" & ActivityDate <= "2018-04-15") %>% 
  mutate(baby = case_when(ActivityDate < "2018-02-04" ~ "Pre Baby",
                          ActivityDate >= "2018-02-04" ~ "Post Baby"),
         baby = factor(baby, levels = c("Pre Baby", "Post Baby"))
    )

babydata_dailyactivity_grouped <- babydata_dailyactivity %>% 
  group_by(baby) %>% 
  summarise(days = n(),
            totalsteps = sum(TotalSteps),
            totalMVPA = sum(FairlyActiveMinutes + VeryActiveMinutes),
            sumsedentary = sum(SedentaryMinutes),
            meansteps = mean(TotalSteps),
            meanMVPA = mean(FairlyActiveMinutes + VeryActiveMinutes),
            meansedentary = mean(SedentaryMinutes),
            meanrestingHR = mean(RestingHeartRate)
            )

babydata_steps1min<- steps_1min %>% 
  filter(date >= "2017-11-27" & date <= "2018-04-15")

babydata_sleeplogs <- sleeplogs %>% 
  filter(as.Date(StartTime) >= "2017-11-27" & as.Date(StartTime) <= "2018-04-15") %>% 
  mutate(baby = case_when(as.Date(StartTime) < "2018-02-04" ~ "Pre Baby",
                          as.Date(StartTime) >= "2018-02-04" ~ "Post Baby"),
         baby = factor(baby, levels = c("Pre Baby", "Post Baby"))
  )

babydata_sleeplogs_grouped <- babydata_sleeplogs %>% 
  group_by(baby) %>% 
  summarise(days = n(),
            totalsleepduration = sum(Duration),
            meansleepduration = mean(Duration),
            meandeepsleep = mean(StagesDeepDuration, na.rm = TRUE),
            meenlightsleep = mean(StagesLightDuration, na.rm = TRUE),
            meanREMsleep = mean(StagesREMDuration, na.rm = TRUE),
            meanAwake = mean(StagesWakeDuration, na.rm = TRUE)
  )
  


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

# minute heart rate plot + weekday average  + weekend average
weekend_mean_hr <- ggplot(heartrate_1min, aes(plottime, Value)) +
  geom_point(size = .5, alpha = 0.05, color="grey") + 
  geom_point(data = subset(meanhr, weekday == "Weekday"), aes(plottime, mean), 
             size = .5, alpha = 0.08, color="red") +
  geom_point(data = subset(meanhr, weekday == "Weekend"), aes(plottime, mean), 
             size = .5, color = "yellow") +
scale_x_datetime(breaks=date_breaks("4 hour"), labels = date_format("%H:%M", tz = "")) +
  labs(title = "Weekend Mean Heart Rate ", 
       subtitle = "October 5, 2016 - April 20, 2018",
       x = "Time of Day", 
       y = "Heart Rate (beats per minute)") +
  theme_ipsum_rc()

ggsave("weekendmeanhr.tiff", width = 13.33, height = 7.5, units = "in")


# baby data steps difference

baby_meanSteps <- ggplot(babydata_dailyactivity_grouped, aes(baby, meansteps, fill = baby)) +
  geom_col() +
  scale_y_continuous(labels = comma, limits = c(0, 12000)) +
  labs(title = "Impact of Parenthood on \nDaily Steps",
       subtitle = "mean daily step count 70 days pre/post",
       x = "",
       y = "Mean Daily Steps",
       fill = ""
  ) +
  theme_ipsum_rc()

ggsave("babymeanstepsdiff.tiff", width = (13.33/3), height = 7.5, units = "in")

baby_meanMVPA <- ggplot(babydata_dailyactivity_grouped, aes(baby, meanMVPA, fill = baby)) +
  geom_col() +
  scale_y_continuous(labels = comma, limits = c(0, 65)) +
  labs(title = "Impact of Parenthood on \nMVPA Minutes",
       subtitle = "mean MVPA minutes 70 days pre/post",
       x = "",
       y = "Mean Daily MVPA Minutes",
       fill = ""
  ) +
  theme_ipsum_rc()

ggsave("babymeanMVPAdiff.tiff", width = (13.33/3), height = 7.5, units = "in")

baby_meansedentary <- ggplot(babydata_dailyactivity_grouped, aes(baby, meansedentary, fill = baby)) +
  geom_col() +
  scale_y_continuous(labels = comma, limits = c(0, 700)) +
  labs(title = "Impact of Parenthood on \nSedentary Time",
       subtitle = "mean sedentary minutes 70 days pre/post",
       x = "",
       y = "Mean Daily Sedentary Minutes",
       fill = ""
  ) +
  theme_ipsum_rc()

ggsave("babymeansedentarydiff.tiff", width = (13.33/3), height = 7.5, units = "in")

baby_meanintensity
# baby data daily teps plot
baby_dailysteps <- ggplot(babydata_dailyactivity, aes(ActivityDate, TotalSteps, colour = baby)) +
  geom_line() + 
  geom_point() + 
  geom_smooth() + 
  scale_y_continuous(labels = comma) +
  labs(title = "Impact of Parenthood on Activity",
       subtitle = "daily step count 70 days pre/post",
       x = "Date",
       y = "Daily Total Steps",
       colour = ""
       ) +
  scale_x_date(breaks = date_breaks("1 week"), date_labels = "%m/%d/%y") +
  theme_ipsum_rc() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
                            
ggsave("babydailysteps.tiff", width = 13.33, height = 7.5, units = "in")
                            
