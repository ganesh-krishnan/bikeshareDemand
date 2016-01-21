library (caret)
library (Amelia)
library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)
library (doMC)

source ("support.R")
options (java.parameters = "-Xmx4g")

#registerDoMC (cores=2)
train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df, logTransform = TRUE) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

ctrl <- trainControl(method ="repeatedcv", 
                     number = 5,
                     repeats = 1)

tunegrid <- expand.grid (mtry = c(10), #13
                         numRandomCuts = c(3)) #3

set.seed (1432)
fit <- train (count ~ season + holiday + workingday + weather + temp + atemp +
                      humidity + windspeed + year + month + wday + day + hour,
              data = train.df,
              method = "extraTrees",
              preProcess = c("center", "scale"),
              trControl = ctrl,
              tuneGrid = tunegrid,
              metric = "RMSE",
              maximize = FALSE
)

y.pred <- exp (predict (fit, test.df)) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=y.pred)

write.csv (result.df, "result-extraTrees.csv", quote=FALSE, row.names=FALSE)
