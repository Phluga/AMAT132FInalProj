#Libraries
library(readxl) #To import Excel File
library(fpp2) #To Plot Time Series Data
library(stlplus) #Time Series Decomposition
library(tidyverse) #To convert Excel to Time Series 
library(tseries) #Time Series Functions

#Importing Data Set
data<-read_excel('RainfallData.xlsx')
data

#Converting data set into Time Series
data.ts<-data %>%
  mutate (Month = 1:n()) %>%
  gather (Year, value, -Month) %>%
  arrange (Year, Month) %>%  
  {ts(.$value, start = c(1996, 1), frequency = 12)}  

#Plotting the Time Series
autoplot(data.ts)

#Decomposing time series of data to visualize trend and seasonality
data.stl <- stlplus(data.ts, s.window = "period")
plot(data.stl)

#Creating the training set and test set for the data
training.set <- subset(data.ts, end=length(data.ts)-61)
test.set <- subset(data.ts, start = length(data.ts)-60)

#Augmented Dickey-Fuller (ADF) test to show if time series is stationary or not
adf.test(training.set, k=12)

#p-value of 0.2856>0.05, therefore it is non-stationary
#Perform first level differencing
data.ts.d1 <- diff(training.set, differences = 1)

#p-value of 0.01>0.05, therefore it is stationary
adf.test(data.ts.d1,k=12)

#Check PACF and ACF of to get p and q values for ARIMA
forecast::tsdisplay(data.ts.d1)

#First order differencing was done with p=1 and q=1 thus ARIMA(1,1,1) is considered
ArimaModel <- Arima(y=training.set, order = c(1,1,1))
#ARIMA(1,1,1) has an AICc value of 2918.66
ArimaModel 

#ARIMA(6,0,0) is also considered after trial and error
ArimaModel6 <- Arima(training.set, order=c(6,0,0), lambda=0, include.drift = FALSE,seasonal = list(order=c(6,0,0),period=12)) 
#ARIMA(6,0,0) has an AICc value of 219.89 indicating it is a better model than ARIMA(1,1,1)
ArimaModel6

#ARIMA (6,0,0) on training data
Arima.Model.train <- Arima(training.set, order=c(6,0,0), lambda=0, include.drift = FALSE,seasonal = list(order=c(6,0,0),period=12)) 
Arima.Model.train %>%
  forecast(h=60) %>%
  autoplot()+autolayer(test.set)

#Plot 12-step fitted values to training data
autoplot(training.set, series="Training data") +
  autolayer(fitted(Arima.Model.train, h=12),
            series="12-step fitted values")

#Check ARIMA model to test set of data
Arima.Model.test <- Arima(test.set, model=Arima.Model.train)

#Check the accuracy of the model
accuracy(Arima.Model.test)

#Holt-Winters model on the training data
HW.Model.train <- hw(training.set, seasonal="additive", damped = FALSE, h=60)
HW.Model.train %>%
  forecast(h=60) %>%
  autoplot()+autolayer(test.set)

#Plot 12-step fitted values to training data
autoplot(training.set, series="Training data") +
  autolayer(fitted(HW.Model.train, h=12),
            series="12-step fitted values")

#Check HW model to test set of data
HW.Model.test <- hw(test.set, model=HW.Model.train)

#Check the accuracy of the model
accuracy(HW.Model.test)

#HW model was chosen since it has lower error than ARIMA model
HWModel <- hw(data.ts, seasonal="additive", damped = FALSE, h=60)

#Forecast next 5 years of time series with HW
Five.year.forecast <- forecast(HWModel, h=60)
Five.year.forecast

#Plot Forecast
autoplot(Five.year.forecast) +
  ggtitle("Five-year Forecast for the Monthly Average Precipitation in the Philippines using ARIMA (3,1,4)") +
  xlab("Year") +
  ylab("Rainfall (millimeters)")


