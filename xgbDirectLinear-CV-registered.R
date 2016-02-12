library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)
library (xgboost)

source ("support.R")

train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

train.df$month <- factor (train.df$month)
train.df$year <- factor (train.df$year)

outlierDateTimes <- ymd_hms ("2012-11-07 00:00:00", "2011-01-09 22:00:00")
train.df <- filter (train.df, !datetime %in% outlierDateTimes)

train.formula <-  ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1


trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$registered)
testData <- xgb.DMatrix (model.matrix (train.formula, test.df))

set.seed (4322)
params <- list (booster="gblinear",
                lambda = 0.1,
                alpha = 0.5,
                objective="reg:linear",
                eval_metric="rmse")

fit <- xgb.cv (params, trainData, nround = 10000, nfold = 5, prediction = TRUE, 
               early.stop.round = 5)