#Packages
pacman::p_load(textshape, prophet, ggplot2, urca, stats, aTSA, forecast, imputeTS, fpp2, RMySQL, cellranger, lubridate, DEoptimR, VIM, rio, padr, ggplot2, iClick, dplyr, plotly)
# install.packages("prophet")
# install.packages("prettyunits")
# library("prophet")
# library("prettyunits")
# Grouping the kitchen consumption by different time periods:



## Creating the Time Series for the Kitchen##

kitchents <- list()

kitchents[["monthts"]] <- ts(monthconsumption$Kitchen, frequency = 12, start = c(2007,01))
kitchents[["weekts"]] <- ts(weekconsumption$Kitchen, frequency = 52, start = c(2007,01))
kitchents[["dayts"]] <- ts(dayconsumption$Kitchen, frequency = 365, start = c(2007,01))

# names(kitchents) <- c("monthts", "weekts", "dayts")


## Breaking down into STLR ##

kitchenstl <- list()

for(i in 1:3){
  
  kitchenstl[[i]] <- kitchents[[i]] %>% stl(s.window = "periodic")

}

names(kitchenstl) <- c("monthstl", "weekstl", "daystl")

lapply(kitchenstl, autoplot)

# Importance of each component:


as.numeric(lapply(kitchenstl$monthstl$time.series, var))/var(kitchents[["monthts"]])
as.numeric(lapply(kitchenstl$weekstl$time.series, var))/var(kitchents[["weekts"]])
as.numeric(lapply(kitchenstl$daystl$time.series, var))/var(kitchents[["dayts"]])

# Checking the seasonality

lapply(kitchents, ggseasonplot)

## CHECKING STATIONARITY OF THE TIME SERIES #######

lapply(kitchents, acf)

lapply(kitchents, pacf)

  
# Ljung-Box test:
names <- c("monthts", "weekts", "dayts")

for(i in names){
  
  print(Box.test(kitchents[[i]], type= "Ljung-Box"))
}

lapply(kitchents, adf.test)

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) for level or trend stationarity. P-value < 0,05, means NOT-STATIONARITY DATA
lapply(kitchents, kpss.test)

#Augmented Dickey-Fuller Test Unit Root:

for(i in names){
  
  print(ur.df(kitchents[[i]], type='trend', selectlags = "BIC"))
}

rm(names)

#### We get the best granularity with week consumption. Thus, WE WILL BE WORKING WITH WEEK TIME SERIE:

# Low p-value in Box.test
#Augmented Dickey-Fuller Test (adf.test): stationary
# kpss.test: stationary


# WE HAVE TO REMOVE THE SEASONALITY AND TREND IN ORDER TO MAKE It STATIONARY

ndiffs(kitchents[["monthts"]],type = "trend")

kdifmonthts <- diff(kitchents[["monthts"]])

autoplot(kitchents[["monthts"]] %>% stl(s.window= "periodic"))
autoplot(difmonthts %>% stl(s.window= "periodic"))

kstatmonthts <- kdifmonthts %>% stl(s.window = "periodic") %>% seasadj()

autoplot(kstatmonthts %>% stl(s.window = "periodic"))

ggtsdisplay(kstatmonthts)


################ SPLITTING INTRO TRAINING AND TESTING ##########################
# Spliting the weekly time-serie without the trend and seasonal component
kstatmonthts_train <- window(kstatmonthts, end = c(2010, 01))
kstatmonthts_test <- window(kstatmonthts, start = c(2010, 02))
# Spliting the weekly time-serie with trend and seasonal component
kmonthts_train <- window(kitchents[["monthts"]], end = c(2010, 01))
kmonthts_test <- window(kitchents[["monthts"]], start = c(2010, 02))

######################## ARIMA forecasting ################################

kmod1 <- kstatmonthts_train %>% Arima(order= c(0,0,5))
kmod2 <- kstatmonthts_train %>% Arima(order= c(0,0,8))
kmod3 <- kstatmonthts_train %>% Arima(order= c(7,0,0))
kmod4 <- kstatmonthts_train %>% Arima(order= c(9,0,0))
kmod5 <- kstatmonthts_train %>% Arima(order= c(3,0,0))
kmod6 <- kmonthts_train %>% auto.arima()

autoplot(kstatmonthts_test, color= "black") + 
  autolayer(forecast(kmod1, h= length(kstatmonthts_test)), series= "Arima 0,0,5", PI= FALSE, color= "green")

autoplot(kstatmonthts_test, color= "black") + 
  autolayer(forecast(kmod2, h= length(kstatmonthts_test)), series= "Arima 0,0,8", PI= FALSE, color= "red")

autoplot(kstatmonthts_test, color= "black") + 
  autolayer(forecast(kmod3, h= length(kstatmonthts_test)), series= "Arima 7,0,0", PI= FALSE, color= "blue")

autoplot(kstatmonthts_test, color= "black") + 
  autolayer(forecast(kmod4, h= length(kstatmonthts_test)), series= "Arima 0,0,1", PI= FALSE, color= "yellow")

autoplot(kstatmonthts_test, color= "black") + 
  autolayer(forecast(kmod5, h= length(kstatmonthts_test)), series= "Arima 3,0,0", PI= FALSE, color= "purple")

autoplot(kmonthts_test, color= "black") + 
  autolayer(forecast(kmod6, h= length(kmonthts_test)), series= "Auto-arima", PI= FALSE, color= "brown")

################### HOLT-WINTERS ####################

kmonthts_hw <- kmonthts_train %>% HoltWinters()
# Result of the forecast in the current time-serie
kmonthts_hw$seasonal
# Forecast of 51 months (comparing to Test)
forecast::forecast(kmonthts_hw, h=length(kmonthts_test)) %>% plot(ylab= "Watt-Hours", xlab="Time")

autoplot(kmonthts_test, color= "black") + 
  autolayer(forecast(kmonthts_hw, h= length(kstatmonthts_test)), series= "Holt-Winters", PI= FALSE, color= "green")


################### PROPHET ####################
prophetTS <- monthconsumption[,c(1,3)]

colnames(prophetTS) <- c("ds", "y")

prophet <- prophet(prophetTS)

future <- make_future_dataframe(prophet, periods = 365)

tail(future)

forecast <- predict(prophet, future)

plot(prophet, forecast)

prophet_plot_components(prophet, forecast)

dyplot.prophet(prophet, forecast)



# Checking the accuracy of the models
accuracy(forecast(kmonthts_hw, h=length(kmonthts_test)), kmonthts_test)
accuracy(forecast(kmod6, h= length(kmonthts_test)), kmonthts_test)

checkresiduals(kmonthts_hw)
checkresiduals(kmod6)