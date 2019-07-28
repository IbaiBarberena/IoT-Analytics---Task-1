#Packages
pacman::p_load(ggplot2, urca, stats, aTSA, forecast, imputeTS, fpp2, RMySQL, cellranger, lubridate, DEoptimR, VIM, rio, padr, ggplot2, iClick, dplyr, plotly)

# Grouping the kitchen consumption by different time periods:

monthconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/monthconsumption.rds")

weekconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/weekconsumption.rds")

dayconsumption <- readRDS("C:/Users/Ibai/Desktop/Part_3/Task_1/IoT Analytics - Task 1/Data/dayconsumption.rds")


## Creating the Time Series for the Laundry##

waterts <- list()

waterts[["monthts"]] <- ts(monthconsumption$Water_Heater_AC, frequency = 12, start = c(2007,01))
waterts[["weekts"]] <- ts(weekconsumption$Water_Heater_AC, frequency = 52, start = c(2007,01))
waterts[["dayts"]] <- ts(dayconsumption$Water_Heater_AC, frequency = 365, start = c(2007,01))

## Breaking down into STLR ##

waterstl <- list()

for(i in 1:3){
  
  waterstl[[i]] <- waterts[[i]] %>% stl(s.window = "periodic")
  
}

names(waterstl) <- c("monthstl", "weekstl", "daystl")

lapply(waterstl, autoplot)

# Importance of each component:


as.numeric(lapply(waterstl$monthstl$time.series, var))/var(waterts[["monthts"]])
as.numeric(lapply(waterstl$weekstl$time.series, var))/var(waterts[["weekts"]])
as.numeric(lapply(waterstl$daystl$time.series, var))/var(waterts[["dayts"]])

# Checking the seasonality

lapply(waterts, ggseasonplot)

## CHECKING STATIONARITY OF THE TIME SERIES #######

lapply(waterts, acf)

lapply(waterts, pacf)

# Ljung-Box test:
names <- c("monthts", "weekts", "dayts")

for(i in names){
  
  print(Box.test(waterts[[i]], type= "Ljung-Box"))
}

lapply(waterts, adf.test)

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity. P-value < 0,05, means NOT-STATIONARITY DATA
lapply(waterts, kpss.test)

#Augmented Dickey-Fuller Test Unit Root:

for(i in names){
  
  print(ur.df(waterts[[i]], type='trend', selectlags = "BIC"))
}

rm(names)

# Thus, WE WILL BE WORKING WITH MONTH TIME SERIE:

ndiffs(waterts[["monthts"]],type = "trend")

wdifmonthts <- diff(waterts[["monthts"]])

autoplot(waterts[["monthts"]] %>% stl(s.window= "periodic"))
autoplot(wdifmonthts %>% stl(s.window= "periodic"))

wstatmonthts <- wdifmonthts %>% stl(s.window = "periodic") %>% seasadj()

autoplot(wstatmonthts %>% stl(s.window = "periodic"))

ggtsdisplay(wstatmonthts)

################ SPLITTING INTRO TRAINING AND TESTING ##########################
# Spliting the monthly time-serie without the trend and seasonal component
wstatmonthts_train <- window(wstatmonthts, end = c(2010, 01))
wstatmonthts_test <- window(wstatmonthts, start = c(2010, 02))
# Spwiting the monthwy time-serie with trend and seasonaw component
wmonthts_train <- window(waterts[["monthts"]], end = c(2010, 01))
wmonthts_test <- window(waterts[["monthts"]], start = c(2010, 02))

######################## ARIMA forecasting ################################

wmod1 <- wstatmonthts_train %>% Arima(order= c(2,0,0))
wmod2 <- wstatmonthts_train %>% Arima(order= c(43,0,0))
wmod3 <- wstatmonthts_train %>% Arima(order= c(0,0,43))
wmod4 <- wstatmonthts_train %>% Arima(order= c(0,0,54))
wmod5 <- wmonthts_train %>% auto.arima()

autoplot(wstatmonthts_test, color= "black") + 
  autolayer(forecast(wmod1, h= length(wstatmonthts_test)), series= "Arima 2,0,0", PI= FALSE, color= "green")

autoplot(wstatmonthts_test, color= "black") + 
  autolayer(forecast(wmod2, h= length(wstatmonthts_test)), series= "Arima 6,0,0", PI= FALSE, color= "red")

autoplot(wstatmonthts_test, color= "black") + 
  autolayer(forecast(wmod3, h= length(wstatmonthts_test)), series= "Arima 21,0,0", PI= FALSE, color= "blue")

autoplot(wstatmonthts_test, color= "black") + 
  autolayer(forecast(wmod4, h= length(wstatmonthts_test)), series= "Arima 0,0,2", PI= FALSE, color= "yellow")

autoplot(wmonthts_test, color= "black") + 
  autolayer(forecast(wmod5, h= length(wmonthts_test)), series= "Auto-arima", PI= FALSE, color= "brown")


################### HOLT-WINTERS ####################

wmonthts_hw <- wmonthts_train %>% HoltWinters()
# Result of the forecast in the current time-serie
wmonthts_hw$seasonal
# Forecast of 51 months (comparing to Test)
forecast::forecast(wmonthts_hw, h=length(wmonthts_test)) %>% plot(ylab= "Watt-Hours", xlab="Time")

autoplot(wmonthts_test, color= "black") + 
  autolayer(forecast(wmonthts_hw, h= length(wstatmonthts_test)), series= "Holt-Winters", PI= FALSE, color= "green")

# Checking the accuracy of the models
accuracy(forecast(wmonthts_hw, h=length(wmonthts_test)), wmonthts_test)
accuracy(forecast(wmod5, h= length(wmonthts_test)), wmonthts_test)

checkresiduals(wmonthts_hw)
checkresiduals(wmod5)
