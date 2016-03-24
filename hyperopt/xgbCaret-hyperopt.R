library (caret)
library (lubridate)
library (dplyr)
library (doMC)

source ("support.R")

fitFunc <- function (fitFormula, trainData, method, tuneGrid, ctrl, ...)
{
        fit <- train (fitFormula,
                      data = trainData,
                      method = method,
                      preProcess = c("center", "scale"),
                      trControl = ctrl,
                      tuneGrid = tuneGrid
        )
        
        return (min (fit$results$RMSE))
}

registerDoMC (cores=2)
train.df <- read.csv ("data/train.csv")

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()

set.seed (1432)

fitFormula <- formula (count ~ season + holiday + workingday + weather + temp + atemp +
        humidity + windspeed + year + month + wday + day + hour)

ctrl <- trainControl(method ="repeatedcv", 
                     number = 5,
                     repeats = 1)

tunegrid <- expand.grid (nrounds = c (500, 1000, 1500, 2000),
                        eta = 10^-2, 
                        max_depth = 8,
                        colsample_bytree = 0.8,
                        min_child_weight = 1,
                        gamma = 1)


#fit <- fitFunc (fitFormula, train.df, method="xgbTree", tuneGrid=tunegrid, ctrl=ctrl)
