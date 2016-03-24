suppressMessages (library (caret))
suppressMessages (library (lubridate))
suppressMessages (library (dplyr))
suppressMessages (library (xgboost))

source ("support.R")
trainFile <- "data/trainWith4PrevPreds.csv"
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

train.df <- formatData (train.df) %>% tbl_df()
train.df$month <- factor (train.df$month)
train.df$year <- factor (train.df$year)

train.df <- na.omit (train.df)
#outlierDateTimes <- ymd_hms ("2012-11-07 00:00:00", "2011-01-09 22:00:00")
#train.df <- filter (train.df, !datetime %in% outlierDateTimes)

prevTrainCols <- sapply (1:4, function (x) {paste (trainCol, "prevPred", x, sep="_")})
prevTrainColsPlusSeparated <- paste (prevTrainCols, collapse=" + ")

trainFormula <- paste0 (trainCol, " ~ season + holiday + workingday + weather + temp + atemp +",
                        "humidity + windspeed + year + month + wday + day + hour + ", 
                        prevTrainColsPlusSeparated, " - 1")

trainFormula <- as.formula (trainFormula)

trainData <- xgb.DMatrix (model.matrix (trainFormula, train.df), label=train.df$registered)

# params <- list (eta=0.18542623350463372,
#                 gamma=2.072654565080585,
#                 max_depth=4,
#                 min_child_weight=0.8584180048511384,
#                 subsample=0.8482345927989308,
#                 colsample_bytree=0.7455594667162986)
# 
# fit <- fitFunc (train.formula, trainData, params)