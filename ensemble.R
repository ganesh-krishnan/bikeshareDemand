library (plyr)
library (dplyr)

createEnsemble <- function (resultList) {
        meanCount <- aaply (laply (resultList, as.matrix), c(2), 
               function (x) {mean (as.numeric (x[,2]))})               
        df <- data.frame (datetime=resultList[[1]]$datetime, count=meanCount)
        return (df)
}

files <- c ("result-xgbDirect.csv", "result-extraTrees.csv")

resultList <- lapply (files, function (x) {read.csv (x, header=TRUE, stringsAsFactors = FALSE)})
resultDF <- createEnsemble (resultList)

write.csv (resultDF, "result-ensemble.csv", row.names = FALSE, quote = FALSE)

