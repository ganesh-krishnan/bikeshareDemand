vwPreds <- read.table ("vw/trainPreds.nn", header=FALSE)
y.pred <- exp (vwPreds$V1) - 1
result.df <- data.frame (datetime=strftime (test.df$datetime, 
                                            format="%Y-%m-%d %H:%M:%S", 
                                            tz="UTC"),
                         count=y.pred)
write.csv (result.df, "result-vw.nn.csv", quote=FALSE, row.names=FALSE)