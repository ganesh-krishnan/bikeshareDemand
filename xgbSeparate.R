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

test.df$month <- factor (test.df$month)
test.df$year <- factor (test.df$year)

outlierDateTimes <- ymd_hms ("2012-11-07 00:00:00", "2011-01-09 22:00:00")
train.df <- filter (train.df, !datetime %in% outlierDateTimes)

train.formula <-  ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$casual)
testData <- xgb.DMatrix (model.matrix (train.formula, test.df))

set.seed (4322)

params <- list (booster="gbtree",
                eta=0.00330925962444,
                gamma=0.684530964272,
                max_depth=7,
                min_child_weight=0.596497397942,
                subsample=0.678093555386,
                colsample_bytree=0.662176894972,
                objective="reg:linear",
                eval_metric="rmse")

fit1 <- xgb.train (params, trainData, nround = 8500, nfold = 5)

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$registered)

params <- list (booster="gbtree",
                eta=0.00291713063475,
                gamma=0.00833471795637,
                max_depth=6,
                min_child_weight=1.57952698042,
                subsample=0.626763785155,
                colsample_bytree=0.685032802413,
                objective="reg:linear",
                eval_metric="rmse")

fit2 <- xgb.train (params, trainData, nround = 10000, nfold = 5)

y.pred <- (exp (predict (fit1, testData)) - 1) + (exp (predict (fit2, testData)) - 1)
y.pred[y.pred < 0] = 0
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=y.pred)

write.csv (result.df, "result-xgbSeparate.csv", quote=FALSE, row.names=FALSE)