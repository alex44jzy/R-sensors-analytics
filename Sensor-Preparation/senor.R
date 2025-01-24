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
library(rlist)

data_march = read.csv("/Users/alexjzy/Desktop/R-projects/sensor-data-prep/Assignment_Data.csv")
data_april = read.csv("/Users/alexjzy/Desktop/R-projects/sensor-data-prep/Assignment_Data2.csv")
names(data_march)

data_april %>%
  group_by(unitid) %>%
  summarise(sensorCount = n())

# 1. convert the date_time to "POSIXct" and "POSIXt"
data_march$date_time = ymd_hms(data_march$date_time)
data_april$date_time = ymd_hms(data_april$date_time)

# 2. add a column named date in the end.
data_march$date = as.Date(data_march$date_time)
data_april$date = as.Date(data_april$date_time)

data_march$time = format(data_march$date_time, "%H:%M")
data_april$time = format(data_april$date_time, "%H:%M")

# 3. check duplicated date and time -- no duplicated
df_duplicated = data_march[c("unitid", "date_time")]
data_march[duplicated(df_duplicated), ]

df_duplicated = data_april[c("unitid", "date_time")]
data_april[duplicated(df_duplicated), ]

# 4. check missing data
all_time_seq = unique(data_march$time)

allDate = unique(data_march$date)

data_march %>%
  split(.$date) %>%
  



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



