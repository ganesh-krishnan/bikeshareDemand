library (caret)
library (Amelia)
library (plyr)
library (dplyr)
library (ggplot2)

source ("support.R")

train.df <- read.csv ("data/train.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df()

ctrl <- trainControl(method ="repeatedcv", 
                     number = 10,
                     repeats = 3)

rf.fit <- train (count ~ . - casual - registered,
                 method = "rf",
                 preProcess = c("center", "scale"),
                 trControl = ctrl
                 )