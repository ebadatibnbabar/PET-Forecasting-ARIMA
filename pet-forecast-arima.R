#Import Necessary Library
library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(astsa)
library(forecast)
library(readxl)
library(urca)
library(ggfortify)
library(tsutils)
library(writexl)


#Converting To Time Series
bwn_ts <- ts(data = bwn[,2], frequency = 12, start = c(1993,1))
#Selecting Data 
bwn_ts <- window(bwn_ts, start=c(1993,1))

bwn_ts

#Plot Time Series Data
autoplot(bwn_ts) + ylab("PET (mm/month)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', breaks = '4 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Bahawalnagar PET 1993 - 2020")

#Decomposition using stl()
decomp <- stl(bwn_ts[,1], s.window = 'periodic')
#Plot decomposition
autoplot(decomp) + theme_bw() + scale_x_date(date_labels = '%b - %Y', breaks = '4 year', minor_breaks = '2 month') +
  ggtitle("Remainder")

Tt <- trendcycle(decomp)
St <- seasonal(decomp)
Rt <- remainder(decomp)
#Trend Strength Calculation
Ft <- round(max(0,1 - (var(Rt)/var(Tt + Rt))),1)
#Seasonal Strength Calculation
Fs <- round(max(0,1 - (var(Rt)/var(St + Rt))),1)

data.frame('Trend Strength' = Ft , 'Seasonal Strength' = Fs)

#Seasonal Plot
seasonplot(bwn_ts, year.labels = TRUE, col = 1:13, 
           main =  "Bahawalnagar Seasonal Plot", ylab= "PET (mm/month)")

#Seasonal Sub-Series Plot
seasplot(bwn_ts, outplot = 3, trend = FALSE, 
         main = "Seasonal Subseries Plot", ylab= "PET (mm/month)")
#Seasonal Boxplot
seasplot(bwn_ts, outplot = 2, trend = FALSE, 
         main = "Seasonal Box Plot", ylab= "PET (mm/month)")

#Create Train Set
bwn_train <- window(bwn_ts, end = c(2017,12))
#Create Test Set 
bwn_test <- window(bwn_ts, start = c(2018,1))

#Kwiatkowski–Phillips–Schmidt–Shin Test
summary(ur.kpss(bwn_train)) 
#Dickey-Fuller Test
summary(ur.df(bwn_train)) 

#ACF/PACF Plot
acf2(bwn_train)

fit1 <- Arima(bwn_train, order = c(1,1,0), seasonal = c(1,1,0))
fit2 <- Arima(bwn_train, order = c(1,1,2), seasonal = c(1,1,2))
fit3 <- Arima(bwn_train, order = c(0,0,2), seasonal = c(0,0,2))
fit4 <- Arima(bwn_train, order = c(2,0,1), seasonal = c(2,0,1))
fit5 <- Arima(bwn_train, order = c(1,2,1), seasonal = c(1,2,1))
fit6 <- auto.arima(bwn_train, stepwise = FALSE, 
                   approximation = FALSE)


data.frame('Model-1' = fit1$aicc, 'Model-2' = fit2$aicc, 
           'Model-3' = fit3$aicc,
           'Model-4' = fit4$aicc, 
           'Model-5' =  fit5$aicc,'Auto.Arima'= fit6$aicc,
           row.names =   "AICc Value")

checkresiduals(fit2)

#ARIMA Model Accuracy
accuracy(forecast(fit2, h=240), bwn_test)


#Create Model
ARIMA_Model <- Arima(bwn_ts, order = c(1,1,2), 
                     seasonal = c(1,1,2))

#ARIMA Model Forecast
autoplot(forecast(ARIMA_Model, h=252)) + theme_bw() + 
  ylab("PET (mm/month)") + xlab("Datetime") + 
  scale_x_date(date_labels = '%b - %Y', 
               breaks = '4 year', minor_breaks = '2 month') +
  theme_bw() + ggtitle("Bahawalnagar PET ARIMA Forecast 2021-2041")

# ARIMA Model Forecast
ARIMAforecast_data <- forecast(ARIMA_Model, h = 252)

# Convert forecast data to data frame
forecast_df <- as.data.frame(ARIMAforecast_data)

# Save forecast data to Excel
write_xlsx(list(forecast_df = forecast_df), "ARIMAforecast_data.xlsx")