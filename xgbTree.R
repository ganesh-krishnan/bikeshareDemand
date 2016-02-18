library (caret)
library (Amelia)
library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)
library (doMC)

source ("support.R")
source ("xgbFit.R")

train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

train.df$month <- factor (train.df$month)
train.df$year <- factor (train.df$year)

test.df$month <- factor (test.df$month)
test.df$year <- factor (test.df$year)

ctrl <- trainControl(method ="repeatedcv", 
                     number = 5,
                     repeats = 1,
                     savePredictions = "final")

tunegrid <- data.frame (
                nrounds=8500,
                eta=0.00330925962444,
                gamma=0.684530964272,
                max_depth=7,
                min_child_weight=0.596497397942,
                subsample=0.678093555386,
                colsample_bytree=0.662176894972)

set.seed (1432)
fit <- train (registered ~ season + holiday + workingday + weather + temp + atemp +
                         humidity + windspeed + year + month + wday + day + hour,
                data = train.df,
                method = xgbFull,
                preProcess = c("center", "scale"),
                trControl = ctrl,
                tuneGrid = tunegrid
)
