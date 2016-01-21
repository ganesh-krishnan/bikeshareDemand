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

train.formula <-  ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$casual)
testData <- xgb.DMatrix (model.matrix (train.formula, test.df))

set.seed (4322)
params <- list (booster="gbtree",
                eta=0.01,
                gamma=0,
                max_depth=3,
                min_child_weight=1,
                subsample=0.8,
                colsample_bytree=1,
                objective="reg:linear",
                eval_metric="rmse")

fit <- xgb.cv (params, trainData, nround = 10000, nfold = 5, prediction = TRUE, 
               early.stop.round = 3)