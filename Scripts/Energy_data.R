##################### OBTAINING THE DATA USING SQL QUERY #########################

pacman::p_load(RMySQL, cellranger, lubridate, DEoptimR, VIM, rio, padr, ggplot2, iClick, dplyr) # VIM package for aggr function
# install.packages("tidyverse")
# install.packages("VIM")
# install.packages("iClick")
# install.packages("rugarch")
# install.packages("truncnorm")
# install.packages("DistributionUtils")
# library(tidyverse)
# library(VIM)
# library(rugarch)
# library(truncnorm)
# library(DistributionUtils)

# Calendarheat function
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

########### PRACTICING WITH IRIS #################
## Create a database connection
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

# ## List the different tables contained in the database 
# dbListTables(con)
# 
# ## Lists attributes contained in iris table
# dbListFields(con,'iris')
# 
# ## Use asterisk to specify all attributes for download
# irisALL <- dbGetQuery(con, "SELECT * FROM iris")
# 
# ## Use attribute names to specify specific attributes for download
# irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

##################### ENERGY CONSUMPTION DATASET #######################

## ATTRIBUTES OF yr_2006 table ##

# dbListFields(con,'yr_2006')

### DOWNLOADING THE TABLES 2006 THROUGH 2010 ###

yr_2006 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2006")
yr_2007 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2007")
yr_2008 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2008")
yr_2009 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2009")
yr_2010 <- dbGetQuery(con, "SELECT Date, Time, Global_active_power, Global_reactive_power, voltage, global_intensity, Sub_metering_1,  Sub_metering_2, Sub_metering_3 FROM yr_2010")

## INVESTIGATING THE DATAFRAME ##

# str(yr_2010)
# summary(yr_2010)
# head(yr_2010)
# tail(yr_2010)

## COMBINING CONSUMPTION YEARS INTO ONE CONSUMPTION TABLE (only including the DF that span an entire year)##
consumption <- bind_rows(yr_2006, yr_2007, yr_2008, yr_2009, yr_2010)

# class(consumption)
consumption <- as_tibble(consumption)

# Analyzing the data ##

# str(consumption)
# summary(consumption)
# head(consumption)
# tail(consumption)

# Any repeated row? #

# consumption %>% distinct(Date, Time, Global_active_power, Sub_metering_1,Sub_metering_2,Sub_metering_3, .keep_all = TRUE)


##### PREPROCESSING THE DATA #####

### COMBINE DATE AND TIME ATTRIBUTE VALUES IN A NEW ATTRIBUTE COLUMN ###
consumption <-cbind(consumption,paste(consumption$Date,consumption$Time), stringsAsFactors=FALSE)
colnames(consumption)[10] <-"DateTime"

#Moving the columns:
## Move the DateTime attribute within the dataset
consumption <- consumption[,c(ncol(consumption), 1:(ncol(consumption)-1))]
head(consumption)

### CHANGING THE DATETIME TO POSIXCT AND ADDING THE TIME ZONE###

## Convert DateTime from POSIXlt to POSIXct 
consumption$DateTime <- as.POSIXct(consumption$DateTime, "%Y/%m/%d %H:%M:%S")
## Adding Paris time zone
attr(consumption$DateTime, "tzone") <- "Europe/Paris"
consumption$DateTime <- as.POSIXct(consumption$DateTime, "%Y/%m/%d")

# Splitting the DateTime into Date and Time


consumption$realdate <- format(consumption$DateTime, "%Y-%m-%d")
consumption$realtime <- format(consumption$DateTime, "%H:%M")

consumption [,c(2,3)] <- NULL

consumption <- consumption[,c(1,9,10,2,3,4,5,6,7,8)]

colnames(consumption) [2] <- "date"
colnames(consumption) [3] <- "time"

range(consumption$DateTime)
### LUBRIDATE THE DATA ###

# Extracting the year, month, week, weekday of the dataset#

# consumption$year <- year(consumption$DateTime)
# consumption$month <- month(consumption$DateTime)
# consumption$week <- week(consumption$DateTime)
# consumption$weekday <- weekdays(consumption$DateTime)

# unique(consumption$year)

summary(consumption)

#-remove data from year 2006
consumption <- filter(consumption, year(DateTime) != 2006)


## Thicken the dataset ##

consumption_day <- consumption %>%
        thicken('day')

colnames(consumption_day) [11] <- "day"
#Padding the dataset

consumption_pad <- pad(consumption, break_above = 3)


#### DEALING WITH MISSING VALUES ###

anyNA(consumption_pad)

colnames(consumption_pad)[ apply(consumption_pad, 2, anyNA) ]

na_count <-sapply(consumption_pad, function(consumption_pad) sum(length(which(is.na(consumption_pad)))))

na_count <- data.frame(na_count)


################# EXPLORING THE DATA ##########################
consumption_pad$Date <- as.POSIXct(consumption_pad$Date)

## Grouping by day ##
consumption_day2 <- consumption_day %>%
        group_by(day) %>%
        summarize(SSub_metering_1 = sum(Sub_metering_1),
                  SSub_metering_2 = sum(Sub_metering_2),
                  SSub_metering_3 = sum(Sub_metering_3))

consumption_day2pad <- pad(consumption_day2)


consumption_daymonth <- consumption_day2pad %>%
        thicken('month')

consumption_daymonth$day_month <- format(consumption_daymonth$day_month, format="%Y-%m")

# Grouping by month

consumption_daymonth <- consumption_daymonth %>%
  group_by(day_month) %>%
  summarize(SSub_metering_1 = sum(SSub_metering_1),
            SSub_metering_2 = sum(SSub_metering_2),
            SSub_metering_3 = sum(SSub_metering_3))

#Remove the cientific way of showing the y-axis
options(scipen = 99)

ggplot(consumption_daymonth) +
  labs(x='Consumption by day', y='kWh') +
  ggtitle('Total Energy Usage on Submeters 1,2,3') +
  geom_line(aes(x=day_month, y=SSub_metering_1, group= 1, colour='green')) +
  geom_line(aes(x=day_month, y=SSub_metering_2, group= 1, color='red')) +
  geom_line(aes(x=day_month, y=SSub_metering_3, group= 1, color='blue')) +
  labs(colour='SSub_metering')


### CalendarHeat for the missing values ####

#Sum of all the submetering values

consumption$AllSub_meterings <- consumption$Sub_metering_1 + consumption$Sub_metering_2 + consumption$Sub_metering_3

calendarHeat(consumption$date,
             values= consumption$AllSub_meterings,
             varname= "Missing values in energy consumption",
             color= "w2b")



####### COMPARING THE ENERGY CONSUMPTION #########

## Creating a new column with the unrecorded devices ##

consumption$unrecorded <- ((consumption$Global_active_power*1000)/60) - consumption$AllSub_meterings

consumption_pad <- pad(consumption, break_above = 3)

#Energy consumption of the untracked devices

options(scipen = 99)

consumption_unrecorded <- consumption %>% thicken('month')

colnames(consumption_unrecorded) [13] <- "bymonth"

consumption_unrecorded$bymonth <- as.yearmon(consumption_unrecorded$bymonth)

consumption_unrecorded2 <- consumption_unrecorded %>%
  group_by(bymonth) %>%
  summarize(unrecorded = sum(unrecorded),
            Global_active_power = sum(Global_active_power*1000/60),
            AllSub_meterings = sum(AllSub_meterings))

ggplot(consumption_unrecorded2) +
  labs(x='Period', y='kWh consumption') +
  ggtitle('Total energy') +
  geom_line(aes(x=bymonth, y=Global_active_power, group= 1, colour='Total energy')) +
  geom_line(aes(x=bymonth, y=AllSub_meterings, group= 1, colour='Recorded energy by submeterings')) +
  scale_colour_manual(values=c('Unrecorded energy'='red', 'Total energy'='blue', 'Recorded energy by submeterings' = 'green')) +
  labs(colour='Energy consumption') +
  guides(colour=guide_legend(reverse=TRUE)) +
  theme(text = element_text(size = 14))

#### Consumption of global energy use 

consumption_unrecorded3 <- consumption_unrecorded %>%
  group_by(bymonth) %>%
  summarize(total_active_energy = sum(Global_active_power*1000/60))

ggplot(consumption_unrecorded3) +
  labs(x='Period', y='kW-h') +
  ggtitle('Global active usage power - kW-h') +
  geom_line(aes(x= bymonth, y=total_active_energy, group= 1, colour='Energy in kWh')) +
  labs(colour='Total energy')



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
