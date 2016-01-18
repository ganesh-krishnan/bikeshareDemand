library (caret)
library (lubridate)
library (dplyr)
library (xgboost)

source ("support.R")

fitFunc <- function (fitFormula, trainData, params)
{
       
        fit <- xgb.cv(params, booster="gbtree", objective="reg:linear", eval_metric="rmse", 
                      data=trainData, nround = 10000, nfold = 5, early.stop.round = 5,
                      verbose=FALSE)
        
        return (fit$test.rmse.mean[length (fit$test.rmse.mean)])
}

train.df <- read.csv ("data/train.csv")

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()

train.formula <-  ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour - 1

trainData <- xgb.DMatrix (model.matrix (train.formula, train.df), label=train.df$count)

# params <- list (eta=0.18542623350463372,
#                 gamma=2.072654565080585,
#                 max_depth=4,
#                 min_child_weight=0.8584180048511384,
#                 subsample=0.8482345927989308,
#                 colsample_bytree=0.7455594667162986)

#fit <- fitFunc (train.formula, trainData, params)