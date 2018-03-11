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
convertFromTimeToDecimal <- function(time) {
  timedec = sapply(strsplit(time,":"),
         function(x) {
           x <- as.numeric(x)
           round(x[1]+x[2]/60, 3)
         }
  )
  return (timedec)
}


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

# 4. summary every day's sensor performance and figure out the missing values
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

ggplot() + 
  geom_bar(stat = "identity", aes(x = names(missingTimeRecordsByMonth), y = missingTimeRecordsByMonth)) + 
  coord_flip() +
  labs(x = "Date", y = "Missing number", title = "Missing data quantity perday in March and April")


##################################### findings #####################################
# finding
# air conditions

# two months Temperature versus date
data_all %>%
  # filter(month == 3) %>%
  ggplot(aes(x = date_time, y = Temperature)) +
  scale_color_discrete(name = "Day type", labels = c("Weekday", "Weekend")) +
  geom_point(aes(color = as.factor(isWeekend)), size = .3) +
  labs(x = "Date", y = "Temperature (Centigrade)", title = "Temperature versus Date" )
# two months weekday Temperature versus time (24h)
data_weekday %>%
  # filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekday Temperature versus Time" )
  
# two months days Temperature smooth line chart versus time (24h)
data_weekday %>%
  # filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(day))) +
  # geom_line(aes(color = factor(day)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  geom_smooth(aes(color = factor(day))) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekday Temperature (Smooth Line) versus Time" )

# two months weekends Temperature versus Time (24h)
data_weekend %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  # geom_smooth(aes(color = factor(day))) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekend Temperature versus Time" )

# two months weekends Temperature smooth line chart versus Time (24h)
data_weekend %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(day))) +
  scale_x_continuous(breaks = seq(0, 24)) +
  geom_smooth(aes(color = factor(day))) +
  labs(x = "Time (24 hour format)", y = "Temperature(Centigrade)", title = "Weekend Temperature (Smooth Line) versus Time" )

# findings 
# March Opening hours of the building
data_weekday %>%
  filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Light, fill = factor(date))) +
  geom_line(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "March Weekday Luminance versus Time")

# April Opening hours of the building
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Light, fill =factor(date))) +
  geom_line(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "April Weekday Luminance versus Time")

# two months weekends Opening hours of the building
data_weekend %>%
  ggplot(aes(x = timeDec, y = Light, fill =factor(date))) +
  geom_point(aes(color = factor(date)), size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Weekend Luminance versus Time")

# findings
# Abnormal working hours
data_weekday %>%
  filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(aes(color = as.factor(date == "2017-03-03"))) + 
  scale_color_discrete(name = "Date", labels = c("Other weekdays in March", "2017-03-03")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Abnormal working hours in 3nd. March")

# Noise on Mar 3nd.
data_all %>%
  filter(date == '2017-03-03') %>%
  ggplot(aes(x = timeDec, y = Noise)) + 
  geom_line( size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on March 3nd versus Time")

# light on Mar 3nd with different sensors
data_all %>%
  filter(date == '2017-03-03') %>%
  ggplot(aes(x = timeDec, y = Light)) + 
  geom_line(aes(color = unitid), size = .5) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Light on March 3nd versus Time by sensors")

# abnormal on Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(aes(color = as.factor(date == "2017-04-14"))) + 
  scale_color_discrete(name = "Date", labels = c("Other weekdays in April", "2017-04-14")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Abnormal working hours in 14th. April")

# Noise around Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(aes(color = as.factor(date <= '2017-04-14')), size = .3) +
  # geom_smooth(aes(color = factor(date))) +
  # scale_color_discrete(name = "Date", labels = c("Before Apr 14th", "After Apr 14th")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise around April 14th")

# Noise of Apr 16th
data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on April 16th")

# Temperature of Apr 16th
data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Temperature)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Temperature (Centigrade)", title = "Temperature on April 16th")

# Humidity of Apr 16th
data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Humidity)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Humidity", title = "Humidity on April 16th")

# Light of Apr 16th
data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Light)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light (lux)", title = "Light on April 16th")

# Co2 of Apr 16th
data_all %>%
  filter(date == '2017-04-16') %>%
  ggplot(aes(x = timeDec, y = Co2)) +
  geom_line(size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Co2", title = "Co2 on April 16th")


# Noise of 2 monthes March and April
data_all %>%
  ggplot(aes(x = date_time, y = Noise)) + 
  geom_point(aes(color = as.factor(isWeekend)), size = .2) + 
  scale_color_discrete(name = "Day type", labels = c("Weekday", "Weekend")) +
  labs(x = "Date", y = "Noise (dB)", title = "Noise versus Date")

# Noise on Apr 14th
data_weekday %>%
  filter(month == 4) %>%
  ggplot(aes(x = timeDec, y = Noise)) +
  geom_line(aes(color = as.factor(date == '2017-04-14')), size = .3) +
  scale_color_discrete(name = "Day type", labels = c("Weekday", "Weekend")) +
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise (dB)", title = "Noise on April 14th")

data_all %>%
  # filter(month == 4) %>%
  ggplot(aes(x = date_time, y = Light)) +
  geom_point(aes(color = as.factor(month)), size = .5) + 
  # scale_color_discrete(name = "Month", labels = c("March", "April")) +
  labs(x = "Date", y = "Light (lux)", title = "Weekday Luminance versus Date")

# findings
data_weekday %>%
  filter(date == "2017-03-22" & date <= "2017-03-25") %>%
  ggplot(aes(x = timeDec, y = Light)) + 
  geom_line(aes(color = as.factor(date)), size = .3)


## abnormal values exploration - scatterplot

scatterplot_Humidity = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = Humidity, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Humidity", title = "Humidity versus Time") +
  theme(legend.position="none")

scatterplot_Light = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = Light, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Light", title = "Light versus Time") +
  theme(legend.position="none")

scatterplot_Co2 = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "CO2", title = "CO2 versus Time") +
  theme(legend.position="none")

scatterplot_VOC = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = VOC, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "VOC", title = "VOC versus Time") +
  theme(legend.position="none")

scatterplot_Temperature = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = Temperature, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Temperature", title = "Temperature versus Time") +
  theme(legend.position="none")

scatterplot_Noise = data_all %>%
  # filter(date == '2017-03-02') %>%
  ggplot(aes(x = timeDec, y = Noise, fill = factor(date))) +
  geom_point(aes(color = factor(date))) + 
  scale_x_continuous(breaks = seq(0, 24)) + 
  labs(x = "Time (24 hour format)", y = "Noise", title = "Noise versus Time") +
  theme(legend.position="none")

grid.arrange(scatterplot_Humidity, scatterplot_Light, scatterplot_Co2, scatterplot_VOC, scatterplot_Temperature, scatterplot_Humidity, ncol = 3, nrow = 2)



# 3.8 Humidity
humidity = data_all %>%
  # filter(month == 3) %>%
  group_by(timeDec, day) %>%
  summarise(median = median(Humidity)) %>%
  as.data.frame()
humidity$day = factor(humidity$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
humidity %>%
  ggplot(aes(x = timeDec, y = median, fill = day)) +
  geom_line(aes(color = day)) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Humidity", title = "Median Humidity versus Time" )



# 3.9 Carbon Dioxide and VOC on 25th April

data_all %>%
  # filter(date >= '2017-04-21' & date <= '2017-04-27') %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "CO2(ppm)", title = "CO2 versus Time (21th April - 27th April)" )

data_all %>%
  filter(date >= '2017-04-21' & date <= '2017-04-27') %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(date))) +
  geom_line(aes(color = factor(date)), size = .3) +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "CO2(ppm)", title = "CO2 versus Time (21th April - 27th April)" )


# 3.10	Evening Breaks

data_weekday %>%
  # filter(month == 3) %>%
  group_by(timeDec) %>%
  summarise(median = median(Noise)) %>%
  as.data.frame() %>%
  ggplot(aes(x = timeDec, y = median)) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 24)) +
  labs(x = "Time (24 hour format)", y = "Noise(db)", title = "Median Noise versus Time" )







data_all %>%
  filter(month == 3) %>%
  # group_by(timeDec, day) %>%
  # # group_by(unitid, hour) %>%
  # summarise(meanTemp = mean(Temperature),
  #           meanNos = mean(Noise),
  #           meanLgt = mean(Light),
  #           meanCo2 = mean(Co2),
  #           meanVoc = mean(VOC),
  #           meanHdt = mean(Humidity)) %>%
  ggplot(aes(x = timeDec, y = Co2, fill = factor(unitid))) +
  geom_point(aes(color = factor(unitid)), size = .3) 
  # scale_x_continuous(breaks = seq(0, 24), 0.5)
# geom_smooth(aes(color = factor(day)))


data_weekday %>%
  filter(month == 3) %>%
  ggplot(aes(x = timeDec, y = Humidity, fill = factor(date))) +
  geom_point(aes(colour = factor(date)), size = .3) + 
  scale_x_continuous(breaks = seq(0, 24), 0.5)


# noise finding 
data_weekday %>%
  filter(month == 3 ) %>%
  group_by(timeDec) %>%
  # group_by(unitid, hour) %>%
  summarise(meanTemp = mean(Temperature),
            meanNos = mean(Noise),
            meanLgt = mean(Light),
            meanCo2 = mean(Co2),
            meanVoc = mean(VOC),
            meanHdt = mean(Humidity)) %>%
  ggplot(aes(x = timeDec, y = meanNos)) +
  geom_line( size = .3) 


# 4.26 humidity 29 and 31



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

