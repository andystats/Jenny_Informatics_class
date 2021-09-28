library(tidyverse)
library(tidymodels)
library(modeltime)
# library(modeltime.h2o)
library(timetk)
library(lubridate)


mydata <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"), na.strings = c("", "NA"))

State <- "Utah" 
CASES <- mydata %>% 
  filter(Province_State ==State) %>% 
  select(starts_with("X"))

Y<- colSums(CASES) 
n <- as.numeric(length(Y)) 

temp1 <- data.frame(y = Y, x = seq(1:n)) 
temp1$date<-row.names(temp1) 
temp1$Date <- gsub( "X", "", temp1$date)
# temp <- temp1 %>% filter(y > 0) 

temp1$dteday <- as.Date(as.character(temp1$Date), "%m.%d.%y")

temp1$lag <- lag(temp1$y)
temp1$cnt <-  temp1$y - temp1$lag
temp1$cnt[temp1$x==1 ] <- 0

data <- temp1 %>%
  select(dteday, cnt)

data %>% plot_time_series(dteday, cnt)

# Train / test Splits 

splits <- time_series_split(data, 
                            assess = "3 weeks", 
                            cumulative = TRUE)

splits %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(dteday, cnt)


# Model 1: auto_arima ----
model_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(cnt ~ dteday, data = training(splits))

model_arima

# Model 2: arima_boost ----
model_fit_arima_boosted <- arima_boost(
  min_n = 2,
  learn_rate = 0.015
) %>%
  set_engine(engine = "auto_arima_xgboost") %>%
  fit(cnt ~ dteday + as.numeric(dteday) + factor(month(dteday, label = TRUE), ordered = F),
      data = training(splits))


# Model 3: prophet ----
model_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(cnt ~ dteday, data = training(splits))

model_prophet

# Model 4: Machine learning - GLM 
model_glmnet <- linear_reg(penalty = 0.01) %>%  #elastic net penalized regression
  set_engine("glmnet") %>%
  fit(cnt ~ wday(dteday, label = TRUE)
          + month(dteday, label = TRUE)
          + as.numeric(dteday), 
      data = training(splits)
      
      )

model_glmnet

models_tbl <- modeltime_table(
 # model_prophet,
  model_glmnet
)

# Calibrate 
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl

# Accuracy


calibration_tbl %>% modeltime_accuracy()

# Test set visualization 

calibration_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits), 
    actual_data = data
  ) %>%
  plot_modeltime_forecast()


refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = data)

Z <- refit_tbl %>%
  modeltime_forecast(h = "3 months", actual_data = data) 

Z %>%
  plot_modeltime_forecast()






