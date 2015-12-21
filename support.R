formatData <- function (df)
{
        df$datetime <- ymd_hms (df$datetime)
        df$month <- month (df$datetime)
        df$day <- day (df$datetime)
        df
}

computeRMSLE <- function (data, lev=NULL, model=NULL)
{
        if (!is.null (lev) & !is.na (lev))
                stop (paste ("RMSLE metric is only applicable to regression"))
        
        rmsle <- sqrt (mean ((log (1 + data$obs) - log (1 + data$pred))^2))
        names (rmsle) <- "rmsle"
        
        print (rmsle)
        rmsle
}