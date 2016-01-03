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