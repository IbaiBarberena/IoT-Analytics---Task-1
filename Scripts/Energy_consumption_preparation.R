##################### OBTAINING THE DATA USING SQL QUERY #########################

pacman::p_load(ggplot2, urca, stats, aTSA, forecast, imputeTS, fpp2, RMySQL, cellranger, lubridate, DEoptimR, VIM, rio, padr, ggplot2, iClick, dplyr, plotly) 
# VIM package for aggr function
# #urca package for ur.df function
# fpp2 package for forecasting
# ggfortify package for plotting time series
# imputeTS for dealing with missing values
# install.packages("tidyverse")
# install.packages("VIM")
# install.packages("iClick")
# install.packages("rugarch")
# install.packages("truncnorm")
# install.packages("DistributionUtils")
# install.packages("forecast")
# library(forecast)
# library(tidyverse)
# library(VIM)
# library(rugarch)
# library(truncnorm)
# library(DistributionUtils)


## Creating a database connection
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## List the different tables contained in the database
dbListTables(con)

## Lists attributes contained in iris table
dbListFields(con,'iris')

## Use asterisk to specify all attributes for download
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Use attribute names to specify specific attributes for download
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

##################### ENERGY CONSUMPTION DATASET #######################

### DOWNLOADING THE TABLES 2006 THROUGH 2010 ###

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2010")

# ## QUICK LOOK OF THE DATAFRAME ##

str(yr_2010)
summary(yr_2010)
head(yr_2010)
tail(yr_2010)

# ## COMBINING CONSUMPTION YEARS INTO ONE CONSUMPTION TABLE (only including the DF that span an entire year)##
consumption <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

class(consumption)

consumption <- as_tibble(consumption)

# Analyzing the data ##

str(consumption)
summary(consumption)
head(consumption)
tail(consumption)

# Any repeated row? #

consumption %>% distinct(Date, Time, Global_active_power, Sub_metering_1,Sub_metering_2,Sub_metering_3, .keep_all = TRUE)


# ##### PREPROCESSING THE DATA #####

### COMBINE DATE AND TIME ATTRIBUTE VALUES IN A NEW ATTRIBUTE COLUMN ###
consumption <-cbind(consumption,paste(consumption$Date,consumption$Time), stringsAsFactors=FALSE)
colnames(consumption)[10] <-"DateTime"

# #Moving the columns:
## Move the DateTime attribute within the dataset
consumption <- consumption[,c(ncol(consumption), 1:(ncol(consumption)-1))]
head(consumption)


### CHANGING THE DATETIME TO POSIXCT AND ADDING THE TIME ZONE###

## Convert DateTime from POSIXlt to POSIXct 
consumption$DateTime <- as.POSIXct(consumption$DateTime, "%Y/%m/%d %H:%M:%S")
## Adding Paris time zone
attr(consumption$DateTime, "tzone") <- "Europe/Paris"
consumption$DateTime <- as.POSIXct(consumption$DateTime, "%Y/%m/%d")

## Splitting the DateTime into Date and Time

consumption$realdate <- format(consumption$DateTime, "%Y-%m-%d")
consumption$realtime <- format(consumption$DateTime, "%H:%M")

consumption [,c(2,3)] <- NULL

consumption <- consumption[,c(1,9,10,2,3,4,5,6,7,8)]

colnames(consumption) [2] <- "date"
colnames(consumption) [3] <- "time"

range(consumption$DateTime)


# ### LUBRIDATE THE DATA ###

#Remove data from year 2006
consumption <- filter(consumption, year(DateTime) != 2006)


##### Calculating the consumption of Sub-metering 2 ####

# Using only year 2008 #

consumption_s2 <- filter(consumption, year(DateTime) != 2007)
consumption_s2 <- filter(consumption_s2, year(DateTime) != 2009)
consumption_s2 <- filter(consumption_s2, year(DateTime) != 2010)

# Extracting July #
consumption_s2 <- filter(consumption_s2, month(DateTime) == 07)


# Tracking the first 14 days

consumption_s2D <- consumption_s2[c(1:20158),]

consumption_s2D <- consumption_s2D %>% thicken('hour')

consumption_s2D <- consumption_s2D %>%
  group_by(DateTime_hour) %>%
  summarize(Sub_metering2 = sum(Sub_metering_2))

ggplot(consumption_s2D) +
  labs(x='Period', y='kW-h') +
  ggtitle('Energy consumption in Sub2 from 01-07-2008 to 14-07-2008') +
  geom_line(aes(x= DateTime_hour, y=Sub_metering2, group= 1, colour='Consumption in Sub2')) +
  scale_colour_manual(values=c('Consumption in Sub2'='red'))


# ########### CHECKING AND ADJUSTING THE GRANULARITY OF THE DATASET ##############

# plot(consumption$Sub_metering_1)

#We cannot get any useful insight out of this visualization. We need to reduce the number of plots.

consumption$DateTime <- as.Date(consumption$DateTime, "%Y/%m/%d %H:%M:%S")

consumption$year <- year(consumption$DateTime)
consumption$month <- month(consumption$DateTime)
consumption$weekday <- weekdays(consumption$DateTime)
consumption$day <- day(consumption$DateTime)
consumption$hour <- hour(consumption$DateTime)
consumption$minute <- minute(consumption$DateTime)


## Subsetting into a day

houseday <- filter(consumption, year == 2008 & month == 1 & day == 9)

## Plot sub-meter 1
plot_ly(houseday, x = ~houseday$DateTime, y = ~houseday$Sub_metering_2, type = 'scatter', mode = 'lines')

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(houseday, x = ~houseday$DateTime, y = ~houseday$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseday$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseday$Sub_metering_3, name = 'Water Heater | AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


### Calculating the consumption every 10 minutes 
### In this case, I am adding new column by each date/time period, in order to filter the consumption more precisely


# ### Showing the energy consumption per 10 minutes within a day###

houseDay10 <- filter(consumption, year == 2008 & month == 1 & day == 9 & (minute == 0 | minute == 10 | minute == 20 | minute == 30 | minute == 40 | minute == 50))

plot_ly(houseDay10, x = ~houseDay10$DateTime, y = ~houseDay10$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay10$Sub_metering_3, name = 'Water Heater | AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2008-01 every 10 minutes",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# #### Checking the granularity of a week ###


houseweek <- filter(consumption, year == 2008 & month == 1 &  day >= 7 & day <= 13)

plot_ly(houseweek, x = ~houseweek$DateTime, y = ~houseweek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseweek$Sub_metering_3, name = 'Water Heater | AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 7 - 13, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

# There is a rise in the energy consumption in the laundry room during the weekends

# The granularity needs to be adjusted. By 20 mins:

houseweek <- filter(consumption, year == 2008 & month == 1 &  day >= 7 & day <= 13 & (minute == 0 | minute == 20 | minute == 40))

plot_ly(houseweek, x = ~houseweek$DateTime, y = ~houseweek$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseweek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseweek$Sub_metering_3, name = 'Water Heater | AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 7 - 13, 2008 (each 20 min)",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))




################### IMPUTING WITH MISSING VALUES ####################

##### Visualizing the missing values with CalendarHeat ###

# Calendarheat function
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

calendarHeat(consumption$date,
             values= consumption$Sub_metering_1,
             varname= "Missing values in the kitchen",
             color= "w2b")

################# IMPUTING THE MISSING VALUES ###################

# If there are hours in a row missing, then the replacement of the NA will be by 0, as it is supposed that the electricty has been shut off for any reason. So, there has not been any electricity consumption.


nonaconsumption <- pad(consumption, break_above = 3)

(which(is.na(nonaconsumption))/nrow(nonaconsumption))+1 #just to identify a col with NAs

vector_na <- which(is.na(nonaconsumption[,2]))

box <- c()
box_length <- c()
count <- 1
for ( i in 1:length(vector_na) ) {
  i <- i + sum(box_length) - length(box_length)
  if(i > length(vector_na)){stop("you are at the end, my friend")} 
  else{
    if(i == length(vector_na)){box <- c(box, vector_na[i])
    box_length <- c(box_length, count)
    stop("the last NA is isolated")}
  }
  box <- c(box, vector_na[i])
  while(vector_na[i+1] == (vector_na[i] + 1)){count <- count + 1; print(count); i=i+1}
  box_length <- c(box_length, count)
  count = 1
}

na_length_table <- cbind(box,box_length)
na_length_table <- as.data.frame(na_length_table)

#149 minutes or more (rows of missing values) will be replaced by 0 consumption
zerovalues <- which(na_length_table[,2] > 149)

for(j in 2:ncol(nonaconsumption)){
  for(i in zerovalues){
    nonaconsumption[seq(na_length_table[i,1], length= na_length_table[i,2], by=1), j]  <- 0
  }  
}

# If there are less than 149 rows in a row with missing values, then, the values will be replaces by the previous energy consumption:
nonaconsumption <-  na.locf(nonaconsumption)

anyNA(nonaconsumption)

# Saving the Data Frame in a CSV

# saveRDS(nonaconsumption,"C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/nonaconsumption_rds.rds")

# nonaconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/nonaconsumption_rds.rds")

nonaconsumption$DateTime <- ymd_hms(nonaconsumption$DateTime)

variables <- c("Global_active_power", "Global_reactive_power", "voltage", "global_intensity")

nonaconsumption[, variables] <- lapply(nonaconsumption[, variables], as.numeric)

rm(variables)

nonaconsumption$AllSub_meterings <- nonaconsumption$Sub_metering_1 + nonaconsumption$Sub_metering_2 + nonaconsumption$Sub_metering_3
nonaconsumption$unrecorded <- (nonaconsumption$Global_active_power*(1000/60)) - nonaconsumption$AllSub_meterings

# Grouping the consumption by different time periods:

monthconsumption <- nonaconsumption %>% 
  thicken('month') %>% 
  group_by(DateTime_month) %>%  
  summarize(Active_energy = sum((Global_active_power*1000)/60),
            Kitchen = sum(Sub_metering_1),
            Laundry_room = sum(Sub_metering_2),
            Water_Heater_AC = sum(Sub_metering_3),
            All_recorded = sum(AllSub_meterings),
            Unrecorded = sum(unrecorded))

# saveRDS(monthconsumption,"C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/monthconsumption.rds")

weekconsumption <- nonaconsumption %>% 
  thicken('week') %>% 
  group_by(DateTime_week) %>%  
  summarize(Active_energy = sum((Global_active_power*1000)/60),
            Kitchen = sum(Sub_metering_1),
            Laundry_room = sum(Sub_metering_2),
            Water_Heater_AC = sum(Sub_metering_3),
            All_recorded = sum(AllSub_meterings),
            Unrecorded = sum(unrecorded))

# saveRDS(weekconsumption,"C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/weekconsumption.rds")


dayconsumption <- nonaconsumption %>% 
  thicken('day') %>% 
  group_by(DateTime_day) %>%  
  summarize(Active_energy = sum((Global_active_power*1000)/60),
            Kitchen = sum(Sub_metering_1),
            Laundry_room = sum(Sub_metering_2),
            Water_Heater_AC = sum(Sub_metering_3),
            All_recorded = sum(AllSub_meterings),
            Unrecorded = sum(unrecorded))

# saveRDS(dayconsumption,"C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/dayconsumption.rds")


# ## Creating a List with the time series ##

group <- c("month", "week", "day")
gran <- as.list(c()) # or gran <- list()

for(i in 1:length(group)){
  
  gran[[i]]  <- nonaconsumption %>%
    thicken(group[i], colname = "i") %>%
    group_by(i) %>%
    summarize(Active_energy = sum((Global_active_power*1000)/60),
              Kitchen = sum(Sub_metering_1),
              Laundry_room = sum(Sub_metering_2),
              Water_Heater_AC = sum(Sub_metering_3),
              All_recorded = sum(AllSub_meterings),
              Unrecorded = sum(unrecorded))
  
}

lapply(gran, function(x) ggplot(x, aes(Active_energy)) + geom_density())
