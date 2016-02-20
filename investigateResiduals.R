preds1 <- tbl_df(fit$pred)
preds1 <- arrange (preds1, rowIndex)
preds1 <- mutate (preds1, res=obs-pred)
preds1SelectCols <- select (preds1, rowIndex, obs, pred, res) %>% mutate (type="roll")

preds2 <- tbl_df(xgbRegFit$pred)
preds2 <- arrange (preds2, rowIndex)
preds2 <- mutate (preds2, res=obs-pred)
preds2SelectCols <- select (preds2, rowIndex, obs, pred, res) %>% mutate (type="std")


