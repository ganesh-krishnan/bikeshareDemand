xgbFull <- getModelInfo ("xgbTree", regex=FALSE)[[1]]

xgbFull$parameters <- data.frame (parameter = c("nrounds", 
                                                 "max_depth",
                                                 "gamma",
                                                 "eta",
                                                 "min_child_weight",
                                                 "subsample",
                                                 "colsample_bytree"),
                                   class = c("numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric",
                                             "numeric"),
                                   label = c("# Boosting Iterations",
                                             "Max Tree Depth",
                                             "Gamma",
                                             "Shrinkage",
                                             "Min Child Weight",
                                             "Subsample ratio",
                                             "Col Sample ratio"))


xgbFull$grid <- function (x, y, len = NULL, search="grid")
{
  if (search == "grid") {
    out <- expand.grid(max_depth = seq(1, len), 
                       nrounds = floor((1:len)*50),
                       gamma = 0,
                       eta = 0.3,
                       min_child_weight = 1,
                       subsample = 1,
                       colsample_bytree = 1)
  }
  else {
    out <- data.frame(nrounds = sample(1:1000, size = len, replace = TRUE), 
                      max_depth = sample(1:10, replace = TRUE, size = len),
                      gamma = sample (0:10, replace = TRUE, size = len),
                      eta = runif(len, min = 0.001, max = 0.6),
                      min_child_weight = sample (1:5, size = len, replace=TRUE),
                      subsample = runif (len, min = 0.5, max = 1.0),
                      colsample_bytree = runif (len, min = 0.2, max = 1.0))
    
    out$nrounds <- floor(out$nrounds)
    out <- out[!duplicated(out), ]
  }
  out
}

xgbFull$fit <- function (x, y, wts, param, lev, last, classProbs, ...) 
{
  if (is.factor(y)) {
    if (length(lev) == 2) {
      y <- ifelse(y == lev[1], 1, 0)
      dat <- xgb.DMatrix(as.matrix(x), label = y)
      out <- xgb.train(list(eta = param$eta, 
                            max_depth = param$max_depth,
                            gamma = param$gamma,
                            min_child_weight = param$min_child_weight, 
                            subsample = param$subsample,
                            colsample_bytree = param$colsample_bytree),
                       data = dat, 
                       nrounds = param$nrounds, 
                       objective = "binary:logistic", 
                       ...)
    }
    else {
      y <- as.numeric(y) - 1
      dat <- xgb.DMatrix(as.matrix(x), label = y)
      out <- xgb.train(list(eta = param$eta, 
                            max_depth = param$max_depth,
                            gamma = param$gamma,
                            min_child_weight = param$min_child_weight, 
                            subsample = param$subsample,
                            colsample_bytree = param$colsample_bytree),
                       data = dat, num_class = length(lev), 
                       nrounds = param$nrounds, 
                       objective = "multi:softprob", 
                       ...)
    }
  }
  else {
    dat <- xgb.DMatrix(as.matrix(x), label = y)
    out <- xgb.train(list(eta = param$eta, 
                          max_depth = param$max_depth,
                          gamma = param$gamma,
                          min_child_weight = param$min_child_weight, 
                          subsample = param$subsample,
                          colsample_bytree = param$colsample_bytree),
                     data = dat, 
                     nrounds = param$nrounds, 
                     objective = "reg:linear", 
                     ...)
  }
  out
}

xgbFull$loop <- function (grid) 
{
  loop <- ddply(grid, 
                c("eta", "max_depth", "gamma", "min_child_weight", 
                  "subsample", "colsample_bytree"), 
                function(x) c(nrounds = max(x$nrounds)))
  submodels <- vector(mode = "list", length = nrow(loop))
  for (i in seq(along = loop$nrounds)) {
    index <- which(grid$max_depth == loop$max_depth[i] & 
                     grid$eta == loop$eta[i] & 
                     grid$gamma == loop$gamma[i] &
                     grid$min_child_weight == loop$min_child_weight[i] &
                     grid$subsample == loop$subsample[i] &
                     grid$colsample_bytree == loop$colsample_bytree)
    trees <- grid[index, "nrounds"]
    submodels[[i]] <- data.frame(nrounds = trees[trees != 
                                                   loop$nrounds[i]])
  }
  list(loop = loop, submodels = submodels)
}

# tuneGrid <- expand.grid (nrounds = 1000,
#                          eta = 0.001,
#                          gamma = 2,
#                          max_depth = 5,
#                          min_child_weight = 3:5,
#                          subsample = 0.5,
#                          colsample_bytree = 0.5)
# 
# trControl <- trainControl (method = "repeatedcv", 
#                            number = 5, 
#                            repeats = 5, 
#                            verboseIter = TRUE)
# 
# 
# fit <- train (mpg ~ ., 
#               data=mtcars,
#               preProcess = c("center", "scale"), 
#               tuneGrid = tuneGrid,
#               method=xgbFull,
#               verbose = 1)
