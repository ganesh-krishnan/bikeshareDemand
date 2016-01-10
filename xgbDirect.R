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

train.formula <- count ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$count)

params <- list (booster="gbtree",
                eta=0.01,
                gamma=1,
                max_depth=6,
                min_child_weight=1,
                subsample=1,
                colsample_bytree=1,
                objective="reg:linear",
                eval_metric="rmse")

fit <- xgb.cv (params, trainData, 1000, 5, early.stop.round = 3)

y.pred <- exp (predict (fit, test.df)) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=as.integer (y.pred))

write.csv (result.df, "result-xgbTree.csv", quote=FALSE, row.names=FALSE)