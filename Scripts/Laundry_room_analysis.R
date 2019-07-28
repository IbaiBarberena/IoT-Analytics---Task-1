#Packages
pacman::p_load(ggplot2, urca, stats, aTSA, forecast, imputeTS, fpp2, RMySQL, cellranger, lubridate, DEoptimR, VIM, rio, padr, ggplot2, iClick, dplyr, plotly)

# Grouping the kitchen consumption by different time periods:

monthconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/monthconsumption.rds")

weekconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/weekconsumption.rds")

dayconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/dayconsumption.rds")


## Creating the Time Series for the Laundry##

laundryts <- list()

laundryts[["monthts"]] <- ts(monthconsumption$Laundry_room, frequency = 12, start = c(2007,01))
laundryts[["weekts"]] <- ts(weekconsumption$Laundry_room, frequency = 52, start = c(2007,01))
laundryts[["dayts"]] <- ts(dayconsumption$Laundry_room, frequency = 365, start = c(2007,01))

## Breaking down into STLR ##

laundrystl <- list()

for(i in 1:3){
  
  laundrystl[[i]] <- laundryts[[i]] %>% stl(s.window = "periodic")
  
}

names(laundrystl) <- c("monthstl", "weekstl", "daystl")

lapply(laundrystl, autoplot)

# Importance of each component:


as.numeric(lapply(laundrystl$monthstl$time.series, var))/var(laundryts[["monthts"]])
as.numeric(lapply(laundrystl$weekstl$time.series, var))/var(laundryts[["weekts"]])
as.numeric(lapply(laundrystl$daystl$time.series, var))/var(laundryts[["dayts"]])

# Checking the seasonality

lapply(laundryts, ggseasonplot)

## CHECKING STATIONARITY OF THE TIME SERIES #######

lapply(laundryts, acf)

lapply(laundryts, pacf)

# Ljung-Box test:
names <- c("monthts", "weekts", "dayts")

for(i in names){
  
  print(Box.test(laundryts[[i]], type= "Ljung-Box"))
}

lapply(laundryts, adf.test)

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity. P-value < 0,05, means NOT-STATIONARITY DATA
lapply(laundryts, kpss.test)

#Augmented Dickey-Fuller Test Unit Root:

for(i in names){
  
  print(ur.df(laundryts[[i]], type='trend', selectlags = "BIC"))
}

rm(names)


#### We get the best granularity with week consumption. Thus, WE WILL BE WORKING WITH WEEK TIME SERIE:

ndiffs(laundryts[["monthts"]],type = "trend")

ldifmonthts <- diff(laundryts[["monthts"]])

autoplot(laundryts[["monthts"]] %>% stl(s.window= "periodic"))
autoplot(ldifmonthts %>% stl(s.window= "periodic"))

lstatmonthts <- ldifmonthts %>% stl(s.window = "periodic") %>% seasadj()

autoplot(lstatmonthts %>% stl(s.window = "periodic"))

ggtsdisplay(lstatmonthts)

################ SPLITTING INTRO TRAINING AND TESTING ##########################
# Spliting the weekly time-serie without the trend and seasonal component
lstatmonthts_train <- window(lstatmonthts, end = c(2010, 01))
lstatmonthts_test <- window(lstatmonthts, start = c(2010, 02))
# Spliting the monthly time-serie with trend and seasonal component
lmonthts_train <- window(laundryts[["monthts"]], end = c(2010, 01))
lmonthts_test <- window(laundryts[["monthts"]], start = c(2010, 02))

######################## ARIMA forecasting ################################

lmod1 <- lstatmonthts_train %>% Arima(order= c(2,0,0))
lmod2 <- lstatmonthts_train %>% Arima(order= c(6,0,0))
lmod3 <- lstatmonthts_train %>% Arima(order= c(21,0,0))
lmod4 <- lstatmonthts_train %>% Arima(order= c(0,0,2))
lmod5 <- lstatmonthts_train %>% Arima(order= c(0,0,8))
lmod6 <- lmonthts_train %>% auto.arima()

autoplot(lstatmonthts_test, color= "black") + 
  autolayer(forecast(lmod1, h= length(lstatmonthts_test)), series= "Arima 2,0,0", PI= FALSE, color= "green")

autoplot(lstatmonthts_test, color= "black") + 
  autolayer(forecast(lmod2, h= length(lstatmonthts_test)), series= "Arima 6,0,0", PI= FALSE, color= "red")

autoplot(lstatmonthts_test, color= "black") + 
  autolayer(forecast(lmod3, h= length(lstatmonthts_test)), series= "Arima 21,0,0", PI= FALSE, color= "blue")

autoplot(lstatmonthts_test, color= "black") + 
  autolayer(forecast(lmod4, h= length(lstatmonthts_test)), series= "Arima 0,0,2", PI= FALSE, color= "yellow")

autoplot(lstatmonthts_test, color= "black") + 
  autolayer(forecast(lmod5, h= length(lstatmonthts_test)), series= "Arima 0,0,8", PI= FALSE, color= "purple")

autoplot(lmonthts_test, color= "black") + 
  autolayer(forecast(lmod6, h= length(lmonthts_test)), series= "Auto-arima", PI= FALSE, color= "brown")


################### HOLT-WINTERS ####################

lmonthts_hw <- lmonthts_train %>% HoltWinters()
# Result of the forecast in the current time-serie
lmonthts_hw$seasonal
# Forecast of 51 months (comparing to Test)
forecast::forecast(lmonthts_hw, h=length(lmonthts_test)) %>% plot(ylab= "Watt-Hours", xlab="Time")

autoplot(lmonthts_test, color= "black") + 
  autolayer(forecast(lmonthts_hw, h= length(lstatmonthts_test)), series= "Holt-Winters", PI= FALSE, color= "green")

# Checking the accuracy of the models
accuracy(forecast(lmonthts_hw, h=length(lmonthts_test)), lmonthts_test)
accuracy(forecast(lmod2, h= length(lmonthts_test)), lmonthts_test)
accuracy(forecast(lmod5, h= length(lmonthts_test)), lmonthts_test)
accuracy(forecast(lmod6, h= length(lmonthts_test)), lmonthts_test)

checkresiduals(lmonthts_hw)
checkresiduals(lmod2)
checkresiduals(lmod5)
checkresiduals(lmod6)
