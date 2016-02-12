library (caretEnsemble)
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

ctrl <- trainControl(
        method="repeatedcv",
        number=5,
        repeats = 2,
        savePredictions="final",
        verboseIter = TRUE,
        index=createMultiFolds(train.df$count, 5, 2)
)

set.seed (1432)

model_list <- caretList (
        count ~ season + holiday + workingday + weather + temp + atemp +
                humidity + windspeed + year + month + wday + day + hour,
        data = train.df,
        trControl = ctrl,
        tuneList = list(
                xgb = caretModelSpec(method="xgbTree", 
                        tuneGrid=data.frame (
                                nrounds=5000,
                                eta=0.00872705408756,
                                gamma=0.0970527175576,
                                max_depth=7,
                                min_child_weight=2.20181978021,
                                colsample_bytree=0.681358199487
                        ),
                        preProcess = c("center", "scale")
                ),
                xTree = caretModelSpec (method="extraTrees",
                        tuneGrid = data.frame (
                                mtry = 10,
                                numRandomCuts = 3),
                        preProcess = c("center", "scale")
                )
        )
)

glm_ensemble <- caretStack(
        model_list,
        method="enet",
        trControl=trainControl(
                method="boot",
                number=10,
                savePredictions="final"
        )
)

y.pred <- exp (predict (greedy_ensemble, newdata=test.df)) - 1

result.df <- data.frame (
                datetime=strftime (
                        test.df$datetime,
                        format="%Y-%m-%d %H:%M:%S",
                        tz="UTC"
                        ),
                count=y.pred
                )

write.csv (result.df, "result-ensemble.csv", row.names = FALSE, quote = FALSE)
