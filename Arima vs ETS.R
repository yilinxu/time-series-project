### Evan Boyd
### Time Series Project

library(fpp)
library(tseries)
library(ggplot2)
library(forecast)
library(CombMSC)

dat <- read.csv("C:/Users/Evan/Desktop/uChicago/Summer 2020/Time Series/Project/Datasets/GDP.csv")

gdp <- ts(dat$GDP, start = c(1947, 1), frequency = 4, end = c(2020, 4))

autoplot(gdp) +
  labs(x = "Time",
       y = "GDP",
       title = "Plot of GDP, 1947-2020"
  ) 

# auto.arima(gdp, trace = TRUE)
# 
# gdp_corona <- ts(dat$GDP, start = c(1984, 1), frequency = 4, end = c(2020, 2))
# 
# autoplot(gdp_corona)
# 
# auto.arima(gdp_corona, trace = TRUE)
# 
# gdp_modern <- ts(dat$GDP, start = c(1984, 1), frequency = 4, end = c(2020, 2))
# 
# autoplot(gdp_modern)
# 
# auto.arima(gdp_modern, trace = TRUE) #No differences

####Split to train/test

gdp <- ts(dat$GDP, start = c(1947, 1), frequency = 4, end = c(2020, 1))

autoplot(gdp) +
  labs(x = "Time",
       y = "GDP",
       title = "Plot of GDP, 1947-2020, No 2020 Q2-4"
  ) 


split <- splitTrainTest(gdp, numTrain = length(gdp) - 9)

train <- split$train
test <- split$test

arima_train <- auto.arima(train)

tsdisplay(train)

checkresiduals(arima_train)

(pred_forecast <- forecast(arima_train, h = length(test)))

plot(pred_forecast, include = 10, main = "Zoomed plot of Predicted Forecast")


(diff <- test - pred_forecast$mean) 

sum((pred_forecast$mean - test)^2)


###Expontential smoothing

train_ets <- ets(train)

summary(train_ets)

checkresiduals(train_ets)


###SLIDING/EXPANDING WINDOWS

k <- 140
n <- length(gdp)

p <- 4
H <- 6

st <- tsp(gdp)[1]+(k-2)/p #  gives the start time in time units,

mae_arima_expand <- matrix(NA,n-k,H)
mae_arima_slide <- matrix(NA,n-k,H)
mae_ets_expand <- matrix(NA,n-k,H)
mae_ets_slide <- matrix(NA,n-k,H)

rmse_arima_expand <- matrix(NA,n-k,H)
rmse_arima_slide <- matrix(NA,n-k,H)
rmse_ets_expand <- matrix(NA,n-k,H)
rmse_ets_slide <- matrix(NA,n-k,H)

aicc_values <- data.frame(iter = numeric(), aicc_arima_expand = numeric(),
                          aicc_arima_slide = numeric(), aicc_ets_expand = numeric(),
                          aicc_ets_slide = numeric())

for(i in 1:(n-k)) {
  
  ### One Month rolling forecasting
  # Expanding Window 
  train_expand <- window(gdp, end=st + i/p)  ## Window Length: k+i
  
  # Sliding Window - keep the training window of fixed length. 
  # The training set always consists of k observations.
  train_slide <- window(gdp, start=st+(i-k+1)/p, end=st+i/p) ## Window Length: k
  
  test <- window(gdp, start=st + (i+1)/p, end=st + (i+H)/p) ## Window Length: H
  
  fit_1 <- Arima(train_expand, order=c(1,2,2),
                 include.drift=FALSE, lambda=0, method="ML")
  fcast_arima_expand <- forecast(fit_1, h=H)
  
  fit_2 <- Arima(train_slide, order=c(1,2,2), 
                 include.drift=FALSE, lambda=0, method="ML")
  fcast_arima_slide <- forecast(fit_2, h=H)
  
  ets_expand <- ets(train_expand)
  fcast_ets_expand <- forecast(ets_expand, h = H)  
  
  ets_slide <- ets(train_slide)
  fcast_ets_slide <- forecast(ets_slide, h = H)
  
  mae_arima_expand[i,1:length(test)] <- abs(fcast_arima_expand[['mean']]-test)
  mae_arima_slide[i,1:length(test)] <- abs(fcast_arima_slide[['mean']]-test)
  mae_ets_expand[i,1:length(test)] <- abs(fcast_ets_expand[['mean']]-test)
  mae_ets_slide[i,1:length(test)] <- abs(fcast_ets_slide[['mean']]-test)
  
  rmse_arima_expand[i,1:length(test)]<- (fcast_arima_expand[['mean']]-test)^2
  rmse_arima_slide[i,1:length(test)]<- (fcast_arima_slide[['mean']]-test)^2
  rmse_ets_expand[i,1:length(test)]<- (fcast_ets_expand[['mean']]-test)^2
  rmse_ets_slide[i,1:length(test)]<- (fcast_ets_slide[['mean']]-test)^2
  
  aicc_arima_expand <- fcast_arima_expand$model$aicc
  aicc_arima_slide <- fcast_arima_slide$model$aicc
  aicc_ets_expand <- fcast_ets_expand$model$aicc
  aicc_ets_slide <- fcast_ets_slide$model$aicc
  
  add_to_aicc <- data.frame(iter = i, aicc_arima_expand, aicc_arima_slide,
                            aicc_ets_expand, aicc_ets_slide)
  aicc_values <- rbind(aicc_values, add_to_aicc)
  
}


mae_rmse <- data.frame(h = 1:H, mae_arima_expand = colMeans(mae_arima_expand, na.rm = TRUE),
                       mae_arima_slide = colMeans(mae_arima_slide, na.rm = TRUE),
                       mae_ets_expand = colMeans(mae_ets_expand, na.rm = TRUE),
                       mae_ets_slide = colMeans(mae_ets_slide, na.rm = TRUE),
                       rmse_arima_expand = sqrt(colMeans(rmse_arima_expand, na.rm = TRUE)),
                       rmse_arima_slide = sqrt(colMeans(rmse_arima_slide, na.rm = TRUE)),
                       rmse_ets_expand = sqrt(colMeans(rmse_ets_expand, na.rm = TRUE)),
                       rmse_ets_slide = sqrt(colMeans(rmse_ets_slide, na.rm = TRUE)))

colors <- c("Arima Expanding Window" = "darkred", "Arima Sliding Window" = "darkblue", 
            "ETS Expanding Window" = "green", "ETS Sliding Window" = "cyan")

mae_rmse %>% 
  ggplot(aes(x = h)) +
  geom_line(aes(y = mae_arima_expand, color = "Arima Expanding Window")) +
  geom_line(aes(y = mae_arima_slide, color = "Arima Sliding Window")) +  
  geom_line(aes(y = mae_ets_expand, color = "ETS Expanding Window")) +
  geom_line(aes(y = mae_ets_slide, color = "ETS Sliding Window")) +
  scale_color_manual(name = "Model", values = colors) +
  labs(x = "Forecast Horizon",
       y = "MAE",
       color = "Legend",
       title = "Mean Absolute Forecast Error (MAE) vs Forecast Horizon"
  ) 

mae_rmse %>% 
  ggplot(aes(x = h)) +
  geom_line(aes(y = rmse_arima_expand, color = "Arima Expanding Window")) +
  geom_line(aes(y = rmse_arima_slide, color = "Arima Sliding Window")) +  
  geom_line(aes(y = rmse_ets_expand, color = "ETS Expanding Window")) +
  geom_line(aes(y = rmse_ets_slide, color = "ETS Sliding Window")) +
  scale_color_manual(name = "Model", values = colors) +
  labs(x = "Forecast Horizon",
       y = "RMSE",
       color = "Legend",
       title = "Root-square Forecast Error (RMSE) vs Forecast Horizon"
  ) 

aicc_values %>% 
  ggplot(aes(x = iter)) +
  geom_line(aes(y = aicc_arima_expand, color = "Arima Expanding Window")) +
  geom_line(aes(y = aicc_arima_slide, color = "Arima Sliding Window")) +  
  geom_line(aes(y = aicc_ets_expand, color = "ETS Expanding Window")) +
  geom_line(aes(y = aicc_ets_slide, color = "ETS Sliding Window")) +
  scale_color_manual(name = "Model", values = colors) +
  labs(x = "Forecast Horizon",
       y = "AICc",
       color = "Legend",
       title = "AICc vs Iteration Number"
  ) 










