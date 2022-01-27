setwd("~/uni/business/Business_project-main/streaming_users_datasets")

library(readxl)
library(forecast)
library(ggplot2)

pandora <- read_excel("pandora.xlsx", sheet = "Data", 
                      col_types = c("text", "numeric"), na = "NA")
spotify <- read_excel("spotify.xlsx", sheet = "Data", 
                      col_types = c("text", "numeric"), na = "NA")
apple <- read_excel("apple.xlsx", sheet = "Data", 
                    col_types = c("text", "numeric"), na = "NA")
spotify_subs <- read_excel("spotify_subs.xlsx", sheet="Data", 
                           col_types = c("text","numeric"), na="NA")
spotify_revenue <- read_excel("spotify_revenue.xlsx", sheet="Data",
                              col_types=c("text","numeric"))

tsapple <- ts(apple$users, start=c(2015,1),frequency=4)
tsapple
tspandora <- ts(pandora$users, start=c(2013,1),frequency=4)
tspandora
tsspotify <- ts(spotify$users, start=c(2015,1),frequency=4)
tsspotify
tssubs <- ts(spotify_subs$users, start=c(2015,1),frequency=4)
tsdisplay(na.interp(tsapple))
tsdisplay(na.interp(tspandora))
tsdisplay(tsspotify)
tsdisplay(tssubs)


plot(tsspotify, type="b",ylim=c(0,400))
lines(na.interp(tsapple), type="b",col=2)
lines(na.interp(tspandora),col=3,type="b")


## study of number of subscribers on spotify
mod <- tslm(tssubs~trend)
summary(mod)
par(mfrow=c(2,2))
plot(tssubs)
lines(fitted(mod),col=2,lty=2,lwd=1)
plot(residuals(mod))
Acf(residuals(mod))
Pacf(residuals(mod))
par(mfrow=c(1,1))

Arima(tssubs,order = c(1,1,0),seasonal= c(0,1,0))
aa_subs <- auto.arima(tssubs)
aa_subs
Acf(residuals(aa_subs))
Pacf(residuals(aa_subs))

plot(tssubs)
lines(fitted(mod),col=2,lty=2,lwd=2)
lines(fitted(aa_subs),col=3,lt=2,lwd=2)
legend(2015,165,c("linear_model","ARIMA(1,1,0)(0,1,0)[4]"),col = c(2,3),lty=c(2,2),lwd=c(2,2))

autoplot(tssubs,ylab = "Number of subscribers (milions") +
  autolayer(forecast(mod), series = "linear model", alpha=0.5) +
  autolayer(forecast(aa_subs), series = "ARIMA(1,1,0)(0,1,0)[4]", alpha = 0.5) +
  guides(colour = guide_legend("Model"))

# train / test subs
tssubs_train <- window(tssubs,start=2015,end=2019.9)
tssubs_test <- window(tssubs,start=2019.9,end=2021.76)

mod_rmse <- tslm(tssubs_train~trend)
summary(mod_rmse)
linear_model_forecast <- forecast(mod_rmse, h = 7)
lm_rmse <- sqrt(sum(linear_model_forecast$mean - tssubs_test)^2)
lm_rmse

arima_rmse <- Arima(tssubs_train,order = c(1,1,0),seasonal= c(0,1,0))
summary(arima_rmse)
arima_forecast <- forecast(arima_rmse, h = 7)
arima_subs_rmse <- sqrt(sum(arima_forecast$mean - tssubs_test)^2)
arima_subs_rmse

linear_model_forecast$mean
arima_forecast$mean
tssubs_test

## study of spotify's revenue
spotify_revenue
tsrevenue <- ts(spotify_revenue$Revenue, start = c(2016,1), frequency=4)
tsrevenue
tsdisplay(tsrevenue)

lm_revenue <- tslm(tsrevenue~trend)
summary(lm_revenue)
par(mfrow=c(2,2))
plot(tsrevenue)
lines(fitted(lm_revenue),col=2,lty=2,lwd=1)
plot(residuals(lm_revenue))
Acf(residuals(lm_revenue))
Pacf(residuals(lm_revenue))
par(mfrow=c(1,1))

arima_revenue <- Arima(tsrevenue,order=c(0,1,0),seasonal = c(1,1,0))
arima_revenue
aa_revenue <- auto.arima(tsrevenue)
aa_revenue


autoplot(tsrevenue) +
  autolayer(forecast(lm_revenue), series = "linear model", alpha=0.2,lwd=2)+
  autolayer(forecast(arima_revenue), series = "ARIMA(0,1,0)(1,1,0)[4]", alpha=0.2,lwd=2) +
  autolayer(forecast(aa_revenue), series = "ARIMA(0,0,0)(1,1,0)[4] autoarima", alpha = 0.2,lwd=2) +
  guides(colour = guide_legend("Model"))


xregarima_revenue <- Arima(tsrevenue,order=c(0,1,0),seasonal = c(1,1,2),xreg = tssubs[c(-1,-2,-3,-4)]) # gets better with number of subs
xregarima_revenue

autoplot(tsrevenue) +
  autolayer(forecast(xregarima_revenue,xreg = forecast(aa_subs)[[4]]), series = "ARIMA(0,1,0),(1,1,2)[4] xreg AIC=191.17", alpha = 0.75,lwd=2) +
  autolayer(forecast(arima_revenue), series = "ARIMA(0,1,0)(1,1,0)[4] AIC=194.03", alpha=0.2,lwd=2) +
  autolayer(forecast(aa_revenue), series = "ARIMA(0,0,0)(1,1,0)[4] autoarima  AIC=216.03", alpha = 0.2,lwd=2) +
  guides(colour = guide_legend("Model"))

# rmse tests

tsrevenue
tsrevenue_train <- window(tsrevenue, start=2016, end=2020.5)
tsrevenue_train
tsrevenue_test <- window(tsrevenue, start=2020.75, end=2022)
tsrevenue_test

lm_trevenue <- tslm(tsrevenue_train~trend)
summary(lm_trevenue)
arima_trevenue <- Arima(tsrevenue_train,order=c(0,1,0),seasonal = c(1,1,0))
aa_trevenue <- auto.arima(tsrevenue_train)
summary(aa_trevenue)
xregarima_trevenue <- Arima(tsrevenue_train,order=c(0,1,0),seasonal = c(1,1,2),xreg = tssubs[c(-1,-2,-3,-4,-27,-26,-25,-24)],method="ML")

xarima_forecast <- forecast(xregarima_trevenue, xreg = tssubs_test)
aa_forecast <- forecast(aa_trevenue, h=7)
arima_forecast <- forecast(arima_trevenue, h=7)
lm_forecast <- forecast(lm_trevenue, h=7)

rmse_xarima <- sqrt(sum((xarima_forecast$mean - tsrevenue_test)^2))
rmse_aa <- sqrt(sum((aa_forecast$mean - tsrevenue_test)^2))
rmse_arima <- sqrt(sum((arima_forecast$mean - tsrevenue_test)^2))
rmse_lm <- sqrt(sum((lm_forecast$mean - tsrevenue_test)^2))

rmse_xarima
rmse_aa
rmse_arima
rmse_lm

