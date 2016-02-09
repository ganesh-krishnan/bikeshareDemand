library (plyr)
library (dplyr)

generateWeights <- function (numResults = 3, gridSpacing = 0.2) {
        stopifnot (numResults >=1, !is.integer (numResults))
        
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
        
        error <- sapply (weightedResultsList, function (x) {mean (x-resultVector)})
        
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
                        
                        testDF <- df[-x, ]
                        testAvg <- findWeightedAverageForWeightVector (bestWeights, 
                                                                       testDF, resultColName)
                        
                        return (mean (testAvg - resultVector[-x]))
                        
        })
}

files <- c ("result-xgbDirect.csv", "result-extraTrees.csv", "result-vw.nn.csv")

resultList <- lapply (files, function (x) {read.csv (x, header=TRUE, stringsAsFactors = FALSE)})
resultDF <- createEnsemble (resultList)

write.csv (resultDF, "result-ensemble.csv", row.names = FALSE, quote = FALSE)

