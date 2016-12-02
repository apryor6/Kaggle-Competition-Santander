source("project/Santander/lib/dataframe-correlation.R")

weights <- list()
weights[['xgboost']] <- 1
weights[['glmnet']] <- .5
weights[['gbm']] <- .5

test.glmnet <- fread("caret_glmnet_preds_test.csv")
val.glmnet  <- as.data.frame(fread("caret_glmnet_preds_val.csv"))

val.gbm  <- as.data.frame(fread("caret_gbm_preds_val.csv"))
test.gbm  <- as.data.frame(fread("caret_gbm_preds_test.csv"))

test.xgboost <- as.data.frame(fread("xgboost_preds_test.csv"))
val.xgboost  <- as.data.frame(fread("xgboost_preds_val.csv"))
val.xgboost <- val.xgboost[,2:47]
test.xgboost <- test.xgboost[,2:47]

pred.names <- names(val.glmnet)[grepl("\\_pred",names(val.glmnet))]

corr.factor <- df.cor(val.glmnet[,names(val.glmnet) %in% pred.names],val.xgboost[,names(val.xgboost) %in% pred.names])
print(paste("Prediction correlation = ",corr.factor))

corr.factor <- df.cor(val.gbm[,names(val.gbm) %in% pred.names],val.xgboost[,names(val.xgboost) %in% pred.names])
print(paste("Prediction correlation = ",corr.factor))

preds.val <- val.xgboost
preds.val[,names(preds.val)%in% pred.names] <- (weights[["xgboost"]]  * val.xgboost[,names(val.xgboost)%in% pred.names] +
                                                      weights[["gbm"]] *   val.gbm[,names(val.gbm)%in% pred.names]+
                                                      weights[["glmnet"]] *   val.glmnet[,names(val.glmnet)%in% pred.names])

write.csv(preds.val,"combined_preds_val.csv")

preds.test <- test.xgboost
preds.test[,names(preds.test)%in% pred.names] <- (weights[["xgboost"]]  * test.xgboost[,names(test.xgboost)%in% pred.names] +
                                                    weights[["gbm"]] *   test.gbm[,names(test.gbm)%in% pred.names]+
                                                    weights[["glmnet"]] *   test.glmnet[,names(test.glmnet)%in% pred.names])
write.csv(preds.test,"combined_preds_test.csv")