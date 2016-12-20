source("project/Santander/lib/dataframe-correlation.R")

weights <- list()
weights[['single']] <- 1
weights[['multi']] <- .1



test.xgboost <- as.data.frame(fread("xgboost_preds_test.csv"))
val.xgboost  <- as.data.frame(fread("xgboost_preds_val_future.csv"))

test.xgboost.multi <- as.data.frame(fread("xgboost_preds_test_multi.csv"))
val.xgboost.multi  <- as.data.frame(fread("xgboost_preds_val_future_multi.csv"))

pred.names <-names(test.xgboost[grepl("pred",names(test.xgboost))])
product.names <- gsub("\\_pred","",pred.names)
test.xgboost <- test.xgboost %>% 
  select(ncodpers,month.id,one_of(pred.names),one_of(product.names))
test.xgboost.multi <- test.xgboost.multi %>% 
  select(ncodpers,month.id,one_of(pred.names),one_of(product.names))
val.xgboost <- val.xgboost %>% 
  dplyr::select(ncodpers,month.id,one_of(pred.names),one_of(product.names))
val.xgboost.multi <- val.xgboost.multi %>% 
  select(ncodpers,month.id,one_of(pred.names),one_of(product.names))

scale.factor.test <- mean(data.matrix(test.xgboost[,names(test.xgboost) %in% pred.names])) /  mean(data.matrix(test.xgboost.multi[,names(test.xgboost.multi) %in% pred.names]))
test.xgboost.multi[,names(test.xgboost.multi) %in% pred.names] <- test.xgboost.multi[,names(test.xgboost.multi) %in% pred.names] *scale.factor.test
scale.factor.val <- mean(data.matrix(val.xgboost[,names(val.xgboost) %in% pred.names])) /  mean(data.matrix(val.xgboost.multi[,names(val.xgboost.multi) %in% pred.names]))
val.xgboost.multi[,names(val.xgboost.multi) %in% pred.names] <- val.xgboost.multi[,names(val.xgboost.multi) %in% pred.names] *scale.factor.val

# val.xgboost <- val.xgboost[,2:47]
# test.xgboost <- test.xgboost[,2:47]

pred.names <- names(test.xgboost)[grepl("\\_pred",names(test.xgboost))]

# corr.factor <- df.cor(val.xgboost[,names(val.xgboost) %in% pred.names],val.xgboost.multi[,names(val.xgboost.multi) %in% pred.names])
# print(paste("Prediction correlation = ",corr.factor))
# 
# corr.factor <- df.cor(test.xgboost[,names(test.xgboost) %in% pred.names],test.xgboost.multi[,names(test.xgboost.multi) %in% pred.names])
# print(paste("Prediction correlation = ",corr.factor))

preds.val <- val.xgboost
preds.val[,names(preds.val)%in% pred.names] <- (weights[["single"]]  * val.xgboost[,names(val.xgboost)%in% pred.names] +
                                                      weights[["multi"]] *   val.xgboost.multi[,names(val.xgboost.multi)%in% pred.names]) / (weights[['multi']]+weights[['single']])

write.csv(preds.val,"combined_preds_val.csv")

preds.test <- test.xgboost
preds.test[,names(preds.test)%in% pred.names] <- (weights[["single"]]  * test.xgboost[,names(test.xgboost)%in% pred.names] +
                                                    weights[["multi"]] *   test.xgboost.multi[,names(test.xgboost.multi)%in% pred.names]) /  (weights[['multi']]+weights[['single']])
write.csv(preds.test,"combined_preds_test.csv")
