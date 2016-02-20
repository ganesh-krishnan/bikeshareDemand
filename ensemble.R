library (plyr)
library (dplyr)
library (caret)

generateWeights <- function (numResults = 3, gridSpacing = 0.2) {
        stopifnot (numResults >=1, is.integer (numResults))
        
        w <- data.frame (w1=seq (0, 1, gridSpacing))
        if (numResults == 1) return (w)
        
        for (currResult in 2:numResults) {
                wCurrLabel <- paste0 ("w", currResult)
                w <- ddply (w, c(1:currResult - 1), function (x) {
                                if (currResult < numResults)
                                        currWeightVectorAsDF <- data.frame (seq (0, 
                                                1-rowSums (x), gridSpacing))
                                else
                                        currWeightVectorAsDF <- data.frame (1-rowSums (x))
                                colnames (currWeightVectorAsDF) <- wCurrLabel
                                return (currWeightVectorAsDF)
                        }
                )
        }
        
        return (w)
}

createEnsemble <- function (resultList) {
        meanCount <- aaply (laply (resultList, as.matrix), c(2), 
               function (x) {mean (as.numeric (x[,2]))})               
        df <- data.frame (datetime=resultList[[1]]$datetime, count=meanCount)
        return (df)
}

createCompositeDF <- function (resultList, resultColName) {
        #Check if all DFs in List have the same columns
        lapply (resultList, function (x) {
                if (!is.data.frame (x)) stop ("resultList should be a list of data frames")
                }
        )
        
        columnNames <- names (resultList[[1]])
        
        #The result column needs to be removed as a join variable
        
        columnsToJoinBy <- columnNames[!columnNames %in% resultColName]
        
        compositeDF <- join_all (resultList, columnsToJoinBy)
        
        #The columns in the combined DF will have multiple columns with the same column name
        #Let's name the columns appropriately
        goodColumnNames <- function (joinedDF, resultColName) {
                numResultColsProcessed <- 0
                
                sapply (names (joinedDF), function (x) {
                        if (x==resultColName) {
                                currentColName <- paste0 (resultColName, 
                                                                "_", numResultColsProcessed + 1)
                                numResultColsProcessed <<- numResultColsProcessed + 1
                        }
                        
                        else currentColName <- x
                        
                        return (currentColName)
                })
        }
        
        names (compositeDF) <- goodColumnNames(compositeDF, resultColName)
        return (compositeDF)
}

findWeightedAverageForWeightVector <- function (weightVector, df, resultColName) {
        resultColumns <- names(df)[grep (paste0 (resultColName, "_", "[0-9]+"), colnames (df))]
        resultMatrix <- as.matrix (df[ ,resultColumns])
        
        result <- as.vector (resultMatrix %*% weightVector)
        
        return (result)
}

findWeightedAverageForWeightMatrix <- function (weightMatrix, df, resultColName) {
        weightsList <- split (weightMatrix, seq (nrow (weightMatrix)))
        
        weightsList <- lapply (weightsList, unlist)
        weightedResults <- lapply (weightsList, findWeightedAverageForWeightVector, 
                                        df, resultColName)
        
        return (weightedResults)
}

findBestWeightedAverage <- function (weightMatrix, df, resultColName, resultVector) {
        weightedResultsList <- findWeightedAverageForWeightMatrix (weightMatrix, df, 
                                                                        resultColName)
        
        error <- sapply (weightedResultsList, function (x) {mean ((x-resultVector)^2)})
        
        return (unlist (weightMatrix[which.min (error), ]))
}

findBestCrossValidatedWeightedAverage <- function (weightMatrix, df, resultColName, 
                                                   resultVector, numFolds=5) {
        
        stopifnot (nrow (df)==length (resultVector))
        folds <- caret::createFolds (resultVector, numFolds, returnTrain=TRUE)
        
        lapply (folds, function (x) {
                        trainDF <- df[x, ]
                        bestWeights <- findBestWeightedAverage (weightMatrix, trainDF, 
                                                                resultColName, resultVector[x])
                        
                        print (bestWeights)
                        
                        testDF <- df[-x, ]
                        testAvg <- findWeightedAverageForWeightVector (bestWeights, 
                                                                       testDF, resultColName)
                        
                        return (sqrt (mean ((testAvg - resultVector[-x])^2)))
                        
        })
}

createCrossValidatedEnsemble <- function (trainDF, predictColName, numFolds)
{
        trainFolds <- caret::createFolds (trainDF[[predictColName]], k=numFolds, 
                                          list=TRUE, returnTrain=TRUE)
        
        for (currentFinalHoldOutFold in 1:numFolds) {
                #Hold out current fold
                firstStageFolds <- trainFolds[-currentFinalHoldOutFold]
                
                for (currentFirstStageHoldOutFold in 1:length (firstStageFolds)) {
                        firstStageTrainFolds <- firstStageFolds[-currentFirstStageHoldOutFold]
                        
                }
                
        }
}

files <- c ("models/result-xgbSeparate-train.csv", "models/result-extraTrees-train.csv",
            "models/result-vw-train.csv")

resultList <- lapply (files, function (x) {read.csv (x, header=TRUE, stringsAsFactors = FALSE)})
resultDF <- createCompositeDF (resultList, "count") %>% tbl_df()

for (i in 1:length(files)) {
        varName <- paste0 ("count_", i)
        dots <- lazyeval::interp (~ log (1+a), a=as.name (varName))
        resultDF <- mutate_ (resultDF, .dots=setNames (list (dots), varName))
}

ctrl <- trainControl (method = "repeatedcv",
                      number = 5,
                      repeats = 1,
                      verboseIter = TRUE,
                      savePredictions = TRUE)

tuneGrid <- expand.grid (fraction = seq (0, 1, 0.1), lambda=10^seq (-5, 5, 0.5))

fit <- train (resultDF[,-1], train.df$count, method="xgbLinear", trControl=ctrl)#, tuneGrid = tuneGrid)

files <- c ("result-xgb-registered-prevPreds.csv", "result-xgb-casual-prevPreds.csv")

testList <- lapply (files, function (x) {read.csv (x, header=TRUE, stringsAsFactors = FALSE)})
testDF <- createCompositeDF (testList, "count") %>% tbl_df()

for (i in 1:length(files)) {
        varName <- paste0 ("count_", i)
        dots <- lazyeval::interp (~ log (1+a), a=as.name (varName))
        testDF <- mutate_ (testDF, .dots=setNames (list (dots), varName))
}

y.pred <- exp (predict (fit, testDF[,-1])) - 1
result.df <- data.frame (datetime=strftime (testDF$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=y.pred)

write.csv (result.df, "result-ensemble.csv", row.names = FALSE, quote = FALSE)
# w <- generateWeights (length (files), 0.1)
# findBestCrossValidatedWeightedAverage (w, resultDF, "count", train.df$count)
