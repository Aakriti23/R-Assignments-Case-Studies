#---------------------------------------------------------------------------
#------------------------- Time Series Case Study --------------------------
#---------------------------------------------------------------------------


#---------------------------------------------------------------------------
#------------------------- Setting the directory ---------------------------
#---------------------------------------------------------------------------

getwd()
setwd("F:/Business Analytics/Case studies/Final Case Study-2")


#---------------------------------------------------------------------------
#------------------------- Importing the excel file ------------------------
#---------------------------------------------------------------------------

install.packages("readxl")
require("readxl")
tsdata<-read_xls("UK Outward Passengers Movement.xls")


myts   <- ts(tsdata$Total, start=c(1996, 1), end=c(2005, 4), frequency=4) #Performing for only the total air movement.
plot(myts)


# Performing the time series analysis using Seasonal Decomposition because the data shows seasonality and a trend

#---------------------------------------------------------------------------
#--------------------------- Seasonal Decomposition ------------------------
#---------------------------------------------------------------------------



plot(decompose(myts, type = c("multiplicative")))
?decompose()

fit <- stl(myts, s.window="period")
?stl()
plot(fit)
ls(fit)
print(fit$time.series)
fit$win



install.packages("forecast")
require("forecast")
fit1 <- HoltWinters(myts, beta=FALSE, gamma=TRUE) #Setting gamma=True because there is seasonality in the data.
ls(fit1)

accuracy(fit1$fitted, myts) #We have a MAPE of 3.5%, which is quite good.


# predictive accuracy



fit<-ets(myts)
accuracy(fit$fitted, myts)   # MAPE of 1.94%, which is quite good.
summary(fit)

# predict next three future values

library(forecast)
forecast(fit, 4)
plot(forecast(fit, 4))




