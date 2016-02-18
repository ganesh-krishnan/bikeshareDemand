formatData <- function (df, logTransform=FALSE)
{
        df$datetime <- ymd_hms (df$datetime)
        df$year <- year (df$datetime)
        df$month <- month (df$datetime)
        df$day <- day (df$datetime)
        df$wday <- wday (df$datetime)
        df$hour <- hour (df$datetime)
        df$holiday <- factor (df$holiday, levels=c(0, 1), labels=c("no", "yes"))
        df$workingday <- factor (df$workingday, levels=c(0,1), labels=c("no", "yes"))
        df$weather <- factor (df$weather, levels=c(1:4), 
                              labels=c("clear", "cloudy", "lightRainOrSnow", "heavyRain"))
        df$season <- factor (df$season, levels=c(1:4), 
                             labels=c("spring", "summer", "fall", "winter"))
                
        if (logTransform == TRUE) 
        {
                df$count <- log (df$count + 1)
                df$casual <- log (df$casual + 1)
                df$registered <- log (df$registered + 1)
        }
        
        df
}

getPrevPreds <- function (df, lookupValue, window=4, lookupColumn="datetime", 
                          valueColumn="registered")
{
        df <- tbl_df (df)
        dots <- list (lazyeval::interp (~lookupColumn, lookupColumn=as.name (lookupColumn)))
        dots <- c(dots, lazyeval::interp (~ valueColumn, valueColumn=as.name (valueColumn)))
        
        df <- select_(df, .dots=dots)
        lowerLookupBound <- lookupValue - dhours (window)
        upperLookupBound <- lookupValue - dhours (1)
        
        dots <- lazyeval::interp (~ lookupColumn >= lowerLookupBound & 
                                lookupColumn <= upperLookupBound, 
                                lookupColumn=as.name (lookupColumn),
                                lowerLookupBound=lowerLookupBound,
                                upperLookupBound=upperLookupBound)
        
        filteredDF <- filter_ (df, dots)
        prevPreds <- filteredDF[[valueColumn]]
        if (length (prevPreds) < window) {
            if (length (prevPreds) > 0) {
                prevPreds <- c(rep (mean (prevPreds, na.rm=TRUE), 
                                    window-length (prevPreds)), 
                               prevPreds)
            } else {
                prevPreds <- rep (NA, window)
            }
        }
        return (prevPreds)
}

createDFWithPrevPreds <- function (df, window=4, lookupColumn="datetime",
                                   valueColumn="registered")
{
        columnNames <- paste (valueColumn, "prevPred", c(window:1), sep="_")
        prevPredsDF <- tapply (df[[lookupColumn]], 1:length (df[[lookupColumn]]), 
                        function (currentLookupValue) {
                                prevPreds <- getPrevPreds (df, currentLookupValue, window, 
                                                           lookupColumn, valueColumn)
                                
                                retDF <- data.frame (rbind (prevPreds))
                                colnames (retDF) <- columnNames
                                return (retDF)
                        })
        
        rbind_all (prevPredsDF)
}

computeRMSLE <- function (data, lev=NULL, model=NULL)
{
        if (!is.null (lev) & !is.na (lev))
                stop (paste ("RMSLE metric is only applicable to regression"))
        
        if (any (data$pred < 0)) warning ("RMSLE: Negative predictions found") 
                
        data$pred[data$pred < 0] <- 0
        
        rmsle <- sqrt (mean ((log (1 + data$obs) - log (1 + data$pred))^2))
        names (rmsle) <- "rmsle"
        
        print (rmsle)
        rmsle
}