suppressMessages (library (caret))
suppressMessages (library (lubridate))
suppressMessages (library (dplyr))
suppressMessages (library (xgboost))

source ("../support.R")
trainFile <- "../data/train.csv"
trainCol <- "registered"

fitFunc <- function (fitFormula, trainData, params)
{
       
        set.seed (4322)
        fit <- xgb.cv(params, booster="gbtree", objective="reg:linear", eval_metric="rmse", 
                      data=trainData, nround = 10000, nfold = 5, early.stop.round = 5,
                      verbose=FALSE)
        
        return (fit$test.rmse.mean[length (fit$test.rmse.mean)])
}

train.df <- read.csv (trainFile)

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()
train.df$month <- factor (train.df$month)
train.df$year <- factor (train.df$year)

trainFormula <- paste0 (trainCol, " ~ season + holiday + workingday + weather + temp + atemp +",
                        "humidity + windspeed + year + month + wday + day + hour")

trainFormula <- as.formula (trainFormula)

trainData <- xgb.DMatrix (model.matrix (trainFormula, train.df), label=train.df$registered)