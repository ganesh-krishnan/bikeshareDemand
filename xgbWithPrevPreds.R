library (caret)
library (Amelia)
library (plyr)
library (dplyr)
library (ggplot2)
library (lubridate)

source ("support.R")
source ("xgbFit.R")

trainCol <- "casual"
train.df <- read.csv ("data/trainWith4PrevPreds.csv")
test.df <- read.csv ("data/test.csv")

train.df <- formatData (train.df) %>% tbl_df()
test.df <- formatData (test.df) %>% tbl_df() %>% arrange (datetime)

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
prevTrainCols <- sapply (1:4, function (x) {paste (trainCol, "prevPred", x, sep="_")})
prevTrainColsPlusSeparated <- paste (prevTrainCols, collapse=" + ")

trainFormula <- paste0 (trainCol, "~ season + holiday + workingday + weather + temp + atemp +",
        "humidity + windspeed + year + month + wday + day + hour + ", prevTrainColsPlusSeparated)

trainFormula <- as.formula (trainFormula)

fit <- train (trainFormula,
              data = train.df,
              method = xgbFull,
              preProcess = c("center", "scale"),
              trControl = ctrl,
              tuneGrid = tunegrid
)

missingCols <- colnames (train.df)[!colnames (train.df) %in% colnames (test.df)]
for (currentColName in missingCols) test.df[[currentColName]] <- NA

composite.df <- rbind (train.df, test.df)
preds <- rep (NA, nrow (test.df))

for (currentRow in 1:nrow (test.df)) {
        print (currentRow)        
        currentDataRow <- test.df[currentRow,]
        currentDateTime <- currentDataRow[1,]$datetime
        prevPreds <- getPrevPreds(composite.df, currentDateTime, window=4, 
                                  valueColumn=trainCol, impute=TRUE)
        
        relevantCols <- grep (paste0 (trainCol, "_"), colnames (composite.df))
        currentDataRow[1,relevantCols] <- prevPreds
        rowNumberInCompositeDF <- which (composite.df$datetime==currentDateTime)
        preds[currentRow] <- predict (fit, currentDataRow)
        composite.df[rowNumberInCompositeDF, trainCol] <- preds[currentRow]
}

y.pred <- exp (preds) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime,
                                format="%Y-%m-%d %H:%M:%S",
                                tz="UTC"),
                        count=y.pred)

resultFile <- paste0 ("result-xgb-", trainCol, "-prevPreds.csv")
write.csv (result.df, resultFile, quote=FALSE, row.names=FALSE)
