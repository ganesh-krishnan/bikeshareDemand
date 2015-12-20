formatData <- function (df)
{
        df$datetime <- as.POSIXct (df$datetime)
        df
}