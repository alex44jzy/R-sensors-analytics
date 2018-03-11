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
library(gridExtra)
library(corrplot)

# Title: Sensor Data Take Home Assignment          
# Assignment Name: Assignment_DataPrep_IOT
# Course: EB5101 Foundation Business Analytics
# File: Choo Minghui Raymond_Jiang Zhiyuan_Huang Qingyi_Wang Jingli_Yang Chialieh_DataPrep.R

# Team members:
# Student ID  	Name	                
# A0178551X	    Choo Ming Hui Raymond	 
# A0178431A	    Huang Qingyi	        
# A0178415Y	    Jiang Zhiyuan	        
# A0178365R	    Wang Jingli	          
# A0178500J	    Yang Chia Lieh      	

# This assignment codes content including 3 parts: Data Initialization, Data Preparation
# In the codes, the annotation corresponding to the document we submit
# e.g. Findings 3.3 Operating Hours of the Building in annotation => 3.3 Operating Hours of the Building in doc
# e.g. Figure 7: Weekday Luminance versus Time (March) in annotation => Figure 7 in doc

############################################################################################################
##                                                                                                        ##
##                                          Start                                                         ##
##                                                                                                        ##   
############################################################################################################

# initial the repository path to current directory
# !!! Atention: if cannot load the data csv, modify the repository path use setwd()
repository = getwd()
DIR = 'data'
file3 = 'Assignment_Data.csv'
file4 = 'Assignment_Data2.csv'
# load 2 csv files
data_march = read.csv(file.path(DIR, file3))
data_april = read.csv(file.path(DIR, file4))
names(data_march)
summary(data_march)
summary(data_april)


############################################################################################################
##                                                                                                        ##
##                                          Data Initialization                                           ##
##                                                                                                        ##   
############################################################################################################

# 1. convert the date_time to "POSIXct" and "POSIXt"
data_march$date_time = ymd_hms(data_march$date_time)
data_april$date_time = ymd_hms(data_april$date_time)

# Function to modify the date time to numeric date time. e.g. 01:30 -> 1.5
# aimed to plot x-axis display with time of each day.
convertFromTimeToDecimal <- function(time) {
  timedec = sapply(strsplit(time,":"),
         function(x) {
           x <- as.numeric(x)
           round(x[1]+x[2]/60, 3)
         }
  )
  return (timedec)
}

# 2. add column "date", "time", "month", "day of week", "hour", "timeDec", "isWeekend" in the dataset
data_march = data_march %>%
  mutate(date = as.Date(.$date_time)) %>%
  mutate(time = format(.$date_time, "%H:%M")) %>%
  mutate(day = weekdays(.$date)) %>%
  mutate(month = month(.$date)) %>%
  mutate(hour = hour(.$date_time)) %>%
  mutate(timeDec = convertFromTimeToDecimal(.$time)) %>%
  mutate(isWeekend = weekdays(.$date) == 'Saturday' | weekdays(.$date) == 'Sunday')

data_april = data_april %>%
  mutate(date = as.Date(.$date_time)) %>%
  mutate(time = format(.$date_time, "%H:%M")) %>%
  mutate(day = weekdays(.$date)) %>%
  mutate(month = month(.$date)) %>%
  mutate(hour = hour(.$date_time)) %>%
  mutate(timeDec = convertFromTimeToDecimal(.$time)) %>%
  mutate(isWeekend = weekdays(.$date) == 'Saturday' | weekdays(.$date) == 'Sunday')

# 3. cleaned dataset
data_all = data.frame(rbind(data_march, data_april))
data_all$dow = ordered(data_all$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

data_weekday = data_all %>%
  filter(isWeekend != TRUE)
data_weekend = data_all %>%
  filter(isWeekend == TRUE)

############################################################################################################
##                                                                                                        ##
##                                          Data Preparation                                              ##
##                                                                                                        ##   
############################################################################################################

# =================================================== 2.1 ===================================================
# Data Preparation 2.1 Abnormal Data Exploration
# Figure 1: Scatter Plot of Six Environment Parameters versus Time by Day
# abnormal values exploration - scatterplot
scatterplot_Humidity = data_all %>%
  ggplot(aes(x = timeDec, y = Humidity, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Humidity", title = "Humidity versus Time") +
  theme(legend.position="none")

scatterplot_Light = data_all %>%
  ggplot(aes(x = timeDec, y = Light, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light", title = "Light versus Time") +
  theme(legend.position="none")

scatterplot_Co2 = data_all %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "CO2", title = "CO2 versus Time") +
  theme(legend.position="none")

scatterplot_VOC = data_all %>%
  ggplot(aes(x = timeDec, y = VOC, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "VOC", title = "VOC versus Time") +
  theme(legend.position="none")

scatterplot_Temperature = data_all %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Temperature", title = "Temperature versus Time") +
  theme(legend.position="none")

scatterplot_Noise = data_all %>%
  ggplot(aes(x = timeDec, y = Noise, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise", title = "Noise versus Time") +
  theme(legend.position="none")

grid.arrange(scatterplot_Humidity, scatterplot_Light, scatterplot_Co2, scatterplot_VOC, scatterplot_Temperature, scatterplot_Humidity, ncol = 3, nrow = 2)

# ================================================== 2.2 ===================================================
# Data Preparation 2.2 check duplicated date and time -- no duplicated
df_duplicated = data_march[c("unitid", "date_time")]
data_march[duplicated(df_duplicated), ]

df_duplicated = data_april[c("unitid", "date_time")]
data_april[duplicated(df_duplicated), ]

# =================================================== 2.3 ==================================================
# Data Preparation 2.3	Missing Data Exploration
# Figure 2: Plot of Missing Data Quantity by Day
# summary every day's sensor performance and figure out the missing values
summary_data_all_sensors = data_all %>%
  group_by(unitid, date) %>%
  summarise(count = n()) %>%
  as.data.frame() %>%
  spread("unitid", "count")
summary_data_all_sensors

summary_data_all_sensors$Sum = summary_data_all_sensors[, 2:5] %>%
  sapply(function(x) {
    ifelse(is.na(x), 0, x)
  }) %>%
  apply(1, sum) 

all_time_seq = unique(data_march$time)
allDate = unique(data_march$date)

filterMissingTimer = function(data, times) {
  missing = times[!times %in% data$time]
  return (missing)
}

missingTimeRecords = data_all %>%
  split(.$date) %>%
  lapply(filterMissingTimer, all_time_seq) 
missingTimeRecordsByMonth = missingTimeRecords %>%
  sapply(function(x) length(x))

# detials of missing data in March & April
missingTimeRecords

# summary of each data with sensors
summary_data_all_sensors

# Table 2:
# summary each day's missing data 
missingTimeRecordsByMonth

# Figure 2: Plot of Missing Data Quantity by Day
ggplot() + 
  geom_bar(stat = "identity", aes(x = names(missingTimeRecordsByMonth), y = missingTimeRecordsByMonth)) + 
  coord_flip() +
  labs(x = "Date", y = "Missing number", title = "Missing data quantity perday in March and April")


############################################################################################################
##                                                                                                        ##
##                                          Findings                                                      ##
##                                                                                                        ##   
############################################################################################################

# ==================================================== 3.1 ==================================================
# Findings 3.1 Correlations of Variables
# Figure 3: Plot of Correlation of Variables
corrplot(cor(data_all[sapply(data_march, is.numeric)][1:6]), method = "number")

# ==================================================== 3.2 =================================================
# Findings 3.2 Heating, Ventilation and Air-Conditioning (HVAC) System Operating Hours
# Figure 4: Temperature versus Date (March & April)
data_all %>%
  # filter(month == 3) %>%
  ggplot(aes(x = date_time, y = Temperature)) +
  scale_color_discrete(name = "Day of Week", labels = c("Weekday", "Weekend")) +
  geom_point(aes(color = as.factor(isWeekend)), size = .3) +
  labs(x = "Date", y = "Temperature (Centigrade)", title = "Temperature versus Date" )

# two months weekday Temperature versus time (24h)
data_weekday %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekday Temperature versus Time" )
  
# two months days Temperature smooth line chart versus time (24h)
# Figure 5: Weekday Temperature versus Day of Week (March/April)
data_weekday %>%
  ggplot(aes(x = timeDec, y = Temperature)) +
  scale_x_continuous(breaks = seq(0, 24)) +
  scale_color_discrete(name = "Day of Week") +
  geom_smooth(aes(color = dow)) +
  labs(x = "Time (24 hour format)", y = "Temperature (Centigrade)", title = "Weekday Temperature versus Time (Smooth Line)" )

# two months weekends Temperature versus Time (24h)
data_weekend %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  # geom_smooth(aes(color = factor(day))) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekend Temperature versus Time " )

# two months weekends Temperature smooth line chart versus Time (24h)
# Figure 6: Weekend Temperature versus Day of Week (March/April)
data_weekend %>%
  ggplot(aes(x = timeDec, y = Temperature)) +
  scale_x_continuous(breaks = seq(0, 24)) +
  geom_smooth(aes(color = factor(day))) +
  scale_color_discrete(name = "Day of Week") +
  labs(x = "Time (24 hour format)", y = "Temperature (Centigrade)", title = "Weekend Temperature versus Time (Smooth Line)" )


# ====================================================== 3.3 ================================================
# Findings 3.3 Operating Hours of the Building
# Weekday Luminance versus Time (March)
# Figure 7: Weekday Luminance versus Time (March)
data_weekday %>%
  filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Light, fill = factor(date))) +
  geom_line(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  scale_color_discrete(name = "Date") +
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Weekday Luminance versus Time (March)")

# April Opening hours of the building
# Figure 8: Weekday Luminance versus Time (April)
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Light, fill =factor(date))) +
  geom_line(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  scale_color_discrete(name = "Date") +
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Weekday Luminance versus Time (April)")

# two months weekends Opening hours of the building
# Figure 9: Weekend Luminance versus Time (March & April)
data_weekend %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_point(aes(color = factor(date)), size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  scale_color_discrete(name = "Date") +
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Weekend Luminance versus Time (March & April)")


# ===================================================== 3.4 =================================================
# Findings 3.4	Abnormal Lighting Conditions on 3rd March
# Figure 10: Lights left on 3rd March
# Abnormal working hours
data_weekday %>%
  filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(aes(color = as.factor(date == "2017-03-03"))) + 
  scale_color_discrete(name = "Date", labels = c("Other weekdays in March", "2017-03-03")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Luminance on 3rd March and other Weekdays in March versus Time")

# Figure 11: Lighting Conditions on Friday, 3rd March with Sensor Information
# light on Mar 3nd by different sensors
data_all %>%
  filter(date == '2017-03-03') %>%
  ggplot(aes(x = timeDec, y = Light)) + 
  geom_line(aes(color = unitid), size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Luminance on Friday, 3rd March with Sensor Information")

# Figure 12: Noise data on Friday, 3rd March
# Noise on Mar 3nd.
data_all %>%
  filter(date == '2017-03-03') %>%
  ggplot(aes(x = timeDec, y = Noise)) + 
  geom_line( size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on March 3nd versus Time")

# ===================================================== 3.5 =================================================
# Findings 3.5 Varying Light Intensities from March to April
# Figure 13: Luminance versus Date in March & April
data_all %>%
  # filter(month == 4) %>%
  ggplot(aes(x = date_time, y = Light)) +
  geom_point(aes(color = as.factor(isWeekend)), size = .5) + 
  scale_color_discrete(name = "Day type", labels = c("Weekday", "Weekend")) +
  labs(x = "Date", y = "Light (lux)", title = "Luminance versus Date (March & April)")



# ==================================================== 3.6 ==================================================
# Findings 3.6 Abnormal Lighting Conditions on 14th April
# Figure 14: Lighting Conditions on Friday, 14th April
# Abnormal on Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(aes(color = as.factor(date == "2017-04-14"))) + 
  scale_color_discrete(name = "Date", labels = c("Other weekdays in April", "2017-04-14")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Luminance on 14th April and other Weekdays in April versus Time")

# Figure 15: Noise conditions on Friday, 14th April
# Noise on Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(aes(color = as.factor(date == '2017-04-14')), size = .3) +
  scale_color_discrete(name = "Day type", labels = c("Other weekdays in April", "2017-04-14")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on April 14th")

# Noise around Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(aes(color = as.factor(date <= '2017-04-14')), size = .3) +
  # geom_smooth(aes(color = factor(date))) +
  scale_color_discrete(name = "Date", labels = c("Before Apr 14th", "After Apr 14th")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise around April 14th")



# ==================================================== 3.7 ==================================================
# Findings 3.7 Drop in ambient Noise level on 16th April

# Figure 16: Noise versus Date (March & April)
# Abnormal on Apr 16th
# Noise of 2 monthes March and April versus date
data_all %>%
  ggplot(aes(x = date_time, y = Noise)) + 
  geom_point(aes(color = as.factor(isWeekend)), size = .2) + 
  scale_color_discrete(name = "Day type", labels = c("Weekday", "Weekend")) +
  labs(x = "Date", y = "Noise (dB)", title = "Noise versus Date")

# Figure 17: Noise, Temperature, Humidity, Light, CO2, VOC on Sunday, 16th April
# combine all the 6 plots together in one plot, using gridExtra
# add the vertical red line on 6am and 9am.
# Noise of Apr 16th
noise_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on April 16th")
# Temperature of Apr 16th
temperature_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Temperature)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "Temperature (Centigrade)", title = "Temperature on April 16th")
# Humidity of Apr 16th
humidity_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Humidity)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "Humidity (%)", title = "Humidity on April 16th")
# Light of Apr 16th
light_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Light on April 16th")
# Co2 of Apr 16th
co2_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Co2)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "Co2 (ppm)", title = "Co2 on April 16th")
# VOC of Apr 16th
voc_416 = data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Co2)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  geom_vline(xintercept = 6, color = "red") +
  geom_vline(xintercept = 9, color = "red") +
  labs(x = "Time (24 hour format)", y = "VOC (ppm)", title = "VOC on April 16th")
# Plot combining 6 subplots into one grid.
grid.arrange(noise_416, temperature_416, humidity_416, light_416, co2_416, voc_416, ncol = 3, nrow = 2)

# Figure 18: CO2 versus Date (April)
# CO2 versus date (April), add the split line on Apr 16th.
data_all %>%
  filter(month == 4) %>%
  ggplot(aes(x = date_time, y = Co2)) + 
  geom_line(size = .3) +
  geom_vline(xintercept = as_datetime("2017-04-16 06:00:00"), color = "red") + 
  geom_text(aes(as_datetime("2017-04-12 06:00:00"), 500, label = "Apr 16, 6am"), size = 5, colour = "red") + 
  labs(x = "Date", y = "CO2 (ppm)", title = "CO2 versus Date (April)")


# ==================================================== 3.8 ==================================================
# Findings 3.8	Location of Sensors
# Figure 19: Humidity versus Time on Wednesday, 26th April
# sensor locations different 4.26 humidity between different sensors
data_weekday %>%
  filter(date == "2017-04-26") %>%
  ggplot(aes(x = timeDec, y = Humidity)) + 
  geom_line(aes(color = as.factor(unitid)), size = .3) + 
  scale_color_discrete(name = "unitid") +
  labs(x = "Time (24 hour format)", y = "Humidity (%)", title = "Humidity on Wednesday 26th April with Sensor Information")



# =================================================== 3.9 ===================================================
# Findings 3.9 Humidity
# Figure 20: Median Humidity versus Time
humidity = data_all %>%
  group_by(timeDec, day) %>%
  summarise(median = median(Humidity)) %>%
  as.data.frame()
humidity$day = factor(humidity$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
humidity %>%
  ggplot(aes(x = timeDec, y = median, fill = day)) +
  geom_line(aes(color = day)) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Humidity (%)", title = "Median Humidity (March & April) versus Time" )

# =================================================== 3.10 ===================================================
# Findings 3.10 Abnormal Carbon Dioxide and VOC on 25th April
# Figure 21: CO2 versus Time
# Carbon Dioxide and VOC on 25th April
data_all %>%
  # filter(date >= '2017-04-21' & date <= '2017-04-27') %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  scale_color_discrete(name = "Date") +
  labs(x = "Time (24 hour format)", y = "CO2 (ppm)", title = "CO2 (March & April) versus Time " )

# Figure 22: CO2 versus Time (21th April – 27th April)
data_all %>%
  filter(date >= '2017-04-21' & date <= '2017-04-27') %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  scale_color_discrete(name = "Date") +
  labs(x = "Time (24 hour format)", y = "CO2 (ppm)", title = "CO2 (21th April - 27th April) versus Time " )

# =================================================== 3.11 ===================================================
# Findings 3.11 Evening Breaks
# Figure 23: Median Noise versus Time
data_weekday %>%
  # filter(month == 3) %>%
  group_by(timeDec) %>%
  summarise(median = median(Noise)) %>%
  as.data.frame() %>%
  ggplot(aes(x = timeDec, y = median)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Weekday Median Noise (March & April) versus Time" )



