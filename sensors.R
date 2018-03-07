rm(list=ls())
library(ggthemes)
library(tidyverse) # for ggplot, purrr etc.
library(ggplot2)
library(grid)
library(dplyr)
library(tcltk)
library(reshape2)
library(magrittr)
library(lubridate)

# initial the repository path to current directory
repository = getwd()
DIR = 'data'
file3 = 'Assignment_Data.csv'
file4 = 'Assignment_Data2.csv'
data_march = read.csv(file.path(DIR, file3))
data_april = read.csv(file.path(DIR, file4))

names(data_march)

data_april %>%
  group_by(unitid) %>%
  summarise(sensorCount = n())

# 1. convert the date_time to "POSIXct" and "POSIXt"
data_march$date_time = ymd_hms(data_march$date_time)
data_april$date_time = ymd_hms(data_april$date_time)

# 2. add column "date", "time", "month", "weekday" in the end.
data_march = data_march %>%
  mutate(date = as.Date(.$date_time)) %>%
  mutate(time = as.factor(format(.$date_time, "%H:%M"))) %>%
  mutate(day = weekdays(.$date)) %>%
  mutate(month = month(.$date)) %>%
  mutate(hour = hour(.$date_time)) %>%
  mutate(isWeekend = weekdays(.$date) == 'Saturday' | weekdays(.$date) == 'Sunday')

data_april = data_april %>%
  mutate(date = as.Date(.$date_time)) %>%
  mutate(time = as.factor(format(.$date_time, "%H:%M"))) %>%
  mutate(day = weekdays(.$date)) %>%
  mutate(month = month(.$date)) %>%
  mutate(hour = hour(.$date_time)) %>%
  mutate(isWeekend = weekdays(.$date) == 'Saturday' | weekdays(.$date) == 'Sunday')
data_all = data.frame(rbind(data_march, data_april))

data_weekday = data_all %>%
  filter(isWeekend != TRUE)
data_weekend = data_all %>%
  filter(isWeekend == TRUE)

# 3. check duplicated date and time -- no duplicated
df_duplicated = data_march[c("unitid", "date_time")]
data_march[duplicated(df_duplicated), ]

df_duplicated = data_april[c("unitid", "date_time")]
data_april[duplicated(df_duplicated), ]

# 4. check missing data
all_time_seq = unique(data_march$time)
allDate = unique(data_march$date)

filterMissingTimer = function(data, times) {
  missing = times[!times %in% data$time]
  return (missing)
}
missingTimeRecords_march = data_march %>%
  split(.$date) %>%
  lapply(filterMissingTimer, all_time_seq) 
missingStat_march = missingTimeRecords_march %>%
  sapply(function(x) {length(x)})
  
missingTimeRecords_april = data_april %>%
  split(.$date) %>%
  lapply(filterMissingTimer, all_time_seq) 
missingStat_april= missingTimeRecords_april %>%
  sapply(function(x) {length(x)})

missingTimeRecords_march[lapply(missingTimeRecords_march, length) > 0] # March missing data detail
missingTimeRecords_april[lapply(missingTimeRecords_april, length) > 0] # April missing data detail
missingStat_march # March missing data summary
missingStat_april # April missing data summary

ggplot() + 
  geom_bar(stat = "identity", aes(x = names(missingTimeRecords_march), y = missingStat_march)) + 
  coord_flip() +
  labs(x = "Date of March", y = "Missing number", title = "Missing data quantity perday in March")

ggplot() + 
  geom_bar(stat = "identity", aes(x = names(missingTimeRecords_april), y = missingStat_april)) + 
  coord_flip() +
  labs(x = "Date of April", y = "Missing number", title = "Missing data quantity perday in April")


# 5. group by sensorId and date to check the missing date
summary_mar = data_march %>%
group_by(unitid, date) %>%
summarise(count = n()) %>%
as.data.frame() %>%
spread("unitid", "count") 

summary_mar$sum = summary_mar[, 2:5] %>%
  sapply(function(x) {
    ifelse(is.na(x), 0, x)
  }) %>%
  apply(1, sum)

summary_apr = data_april %>%
  group_by(unitid, date) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  spread("unitid", "count") 

summary_apr$sum = summary_apr[, 2:5] %>%
  sapply(function(x) {
    ifelse(is.na(x), 0, x)
  }) %>%
  apply(1, sum)

# findings by alex, 
data_weekday %>%
  filter(month == 4 & day == 'Friday') %>%
  # group_by(unitid, time) %>%
  # summarise(meanTemp = mean(Temperature),
  #           meanNos = mean(Noise),
  #           meanLgt = mean(Light),
  #           meanCo2 = mean(Co2),
  #           meanVoc = mean(VOC),
  #           meanHdt = mean(Humidity)) %>%
  ggplot(aes(x = time, y = Temperature)) +
  geom_point(aes(colour = unitid))



## identify working hours by "Light",boxplot
## comparision of March & April
ggplot(data_all, aes(x=factor(hour), y=Light, fill=factor(month))) + 
  geom_boxplot() + 
  labs(fill = "Month", x = "Time/Hour", y = "Light/Lux", title = "Light by Hour") +
  scale_fill_discrete(labels=c("March","April")) +
  theme(plot.title = element_text(hjust = 0.5))


## light using time during the building's closing time
ab_light = data_all[which(data_all$Light %in% boxplot(Light~hour, data = data_all)$out),]
ab_light %>%
  filter(hour > 2 & hour < 16) %>%
  ggplot(aes(x = date)) + 
  geom_bar() +
  labs(y = "Minutes", title = "Abnormal Light Duration per Day (from 2am-3pm)") +
  theme(plot.title = element_text(hjust = 0.5))

