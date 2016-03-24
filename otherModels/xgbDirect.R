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

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$count)
testData <- xgb.DMatrix (model.matrix (train.formula, test.df))

set.seed (4322)
params <- list (booster="gbtree",
                eta=0.00872705408756,
                gamma=0.0970527175576,
                max_depth=7,
                min_child_weight=2.20181978021,
                subsample=0.571707896984,
                colsample_bytree=0.681358199487,
                objective="reg:linear",
                eval_metric="rmse")

fit <- xgb.train (params, trainData, nround = 5000, nfold = 5)

y.pred <- exp (predict (fit, testData)) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=y.pred)

write.csv (result.df, "result-xgbDirect.csv", quote=FALSE, row.names=FALSE)