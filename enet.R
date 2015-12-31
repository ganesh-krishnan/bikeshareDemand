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

set.seed (4322)

ctrl <- trainControl(method ="repeatedcv", 
                     number = 5,
                     repeats = 1,
                     summaryFunction = computeRMSLE)

tuneGrid <- expand.grid (fraction = seq (0, 1, 0.1), lambda=10^seq (-5, 5, 0.5))

enet.fit <- train (count ~ season + holiday + workingday + weather + temp + atemp +
                         humidity + windspeed + year + month + wday + day + hour,
                 data = train.df,
                 method = "enet",
                 preProcess = c("center", "scale", "zv"),
                 trControl = ctrl,
                 tuneGrid = tuneGrid,
                 metric = "rmsle",
                 maximize = FALSE
)

test.df$casual=1
test.df$registered=1
y.pred <- predict (enet.fit, test.df)
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=as.integer (y.pred)
)

write.csv (result.df, "result-enet.csv", quote=FALSE, row.names=FALSE)
