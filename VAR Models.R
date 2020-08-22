
library(fpp)
library(dplyr)
library(ggplot2)
library(vars)

dat <- read.csv("C:/Users/Evan/Desktop/uChicago/Summer 2020/Time Series/Project/All_Data.csv")

ts_dat <- ts(dat[,2:6], start = c(1967, 3), frequency = 4, end = c(2019, 4))

dat %>% 
  ggplot(aes(GDP, CONS_OP)) +
  geom_jitter() +
  ggtitle("Plot of GDP Versus Consumer Opinion")

cor(dat$GDP, dat$CONS_OP)

cor(dat$UNRATE, dat$GDP)

cor(dat$UNRATE, dat$CONS_OP)

ts_dat[,5] %>% 
  autoplot()

log_cons_op %>% 
  autoplot()

ts_dat[,4] %>% 
  autoplot()

ts_dat[,3] %>% 
  autoplot()

gdp <- ts_dat[,3]
cons_op <- ts_dat[,5]
unemployment <- ts_dat[,4]


###############################VAR Model between gdp and consumer opinion
VARselect(cbind(gdp, cons_op))

var_model <- VAR(cbind(gdp, cons_op), p = 2, type = "both", season = 4)

summary(var_model)

fore_var <- forecast(var_model, h = 12)

autoplot(fore_var)

cons_resid <- var_model$varresult$cons_op
gdp_resid <- var_model$varresult$gdp

##Ljung-Box test
## Ho: Data is independently distributed
## Ha: Data is not independently distributed; exhibit serial correlation

checkresiduals(cons_resid, test = "LB")

Box.test(cons_resid$residuals)

checkresiduals(gdp_resid, test = "LB")
##Significant autocorrelation within the residuals, which means that the prediction
##intervlas may not provide an accurate coverage. 

Box.test(gdp_resid$residuals)
##One significant outlier, two lags, big spike at around 150. This will affect the 
## Coverage of prediction intervals


#####################################VAR model between gdp and unemployment
VARselect(cbind(gdp, unemployment))

var_unrate <- VAR(cbind(gdp, unemployment), p = 3, type = "both", season = 4)

summary(var_unrate)

fore_unrate <- forecast(var_unrate, h = 12)

autoplot(fore_unrate)

unemployment_resid <- var_unrate$varresult$unemployment
gdp_resid <- var_unrate$varresult$gdp


checkresiduals(unemployment_resid, test = "LB")

Box.test(unemployment_resid$residuals)

checkresiduals(gdp_resid, test = "LB")

Box.test(gdp_resid$residuals)
