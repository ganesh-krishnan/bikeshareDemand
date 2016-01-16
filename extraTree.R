library (caret)
library (Amelia)
library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)
library (doMC)

source ("support.R")

#registerDoMC (cores=2)
train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

ctrl <- trainControl(method ="repeatedcv", 
                     number = 5,
                     repeats = 1,
                     summaryFunction = computeRMSLE)

tunegrid <- expand.grid (mtry = c(7, 13),
                         numRandomCuts = c(1, 3))

set.seed (1432)
fit <- train (count ~ season + holiday + workingday + weather + temp + atemp +
                      humidity + windspeed + year + month + wday + day + hour,
              data = train.df,
              method = "extraTrees",
              preProcess = c("center", "scale"),
              trControl = ctrl,
              tuneGrid = tunegrid,
              metric = "rmsle",
              maximize = FALSE
)

test.df$casual=1
test.df$registered=1
y.pred <- predict (fit, test.df)
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=as.integer (y.pred))

write.csv (result.df, "result-extraTrees.csv", quote=FALSE, row.names=FALSE)
