library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)
library (xgboost)

source ("support.R")

rmsleObj <- function (preds, dtrain) 
{
        labels <- getinfo(dtrain, "label")
        grad <- -2*log ((preds + 1)/(labels + 1))/(preds + 1)
        hess <- -2/(preds + 1)^2*(1-log ((preds + 1)/(labels + 1)))
        return(list(grad = grad, hess = hess))
}

rmsle <- function(preds, dtrain) 
{
        labels <- getinfo(dtrain, "label")
        
        if (any (preds < 0)) warning ("RMSLE: Negative predictions found")
        preds[preds < 0] <- 0
        
        rmsle <- sqrt (mean ((log (1 + labels) - log (1 + preds))^2))
        names (rmsle) <- "rmsle"
        return(list(metric = "rmsle", value = rmsle))
}

train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

train.formula <-  ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$count)
testData <- xgb.DMatrix (model.matrix (train.formula, test.df))

params <- list (booster="gbtree",
                eta=0.01,
                gamma=1,
                max_depth=6,
                min_child_weight=1,
                subsample=1,
                colsample_bytree=1,
                objective=rmsleObj,
                eval_metric=rmsle)

fit <- xgb.cv (params, trainData, nround = 732, nfold = 5)

y.pred <- exp (predict (fit, testData)) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=as.integer (y.pred))

write.csv (result.df, "result-xgbDirect.csv", quote=FALSE, row.names=FALSE)