source("project/Santander/lib/dataframe-correlation.R")
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')
# weights <- list()
# weights[['single.best']] <- 1
# weights[['multi.best']] <- .1

weight.single.best  <- 1
weight.multi.best   <- .1
weight.single.other <- 0.5
weight.multi.other  <- 0.05

filenames.test <- list("xgboost_preds_test_singleclass_best.csv",
                      "xgboost_preds_test_singleclass_1.csv",
                      "xgboost_preds_test_singleclass_2.csv",
                      "xgboost_preds_test_singleclass_3.csv",
                      "xgboost_preds_test_multiclass_best.csv",
                      "xgboost_preds_test_multiclass_1.csv",
                      "xgboost_preds_test_multiclass_2.csv",
                      "xgboost_preds_test_multiclass_3.csv")


filenames.val <- list("xgboost_preds_val_future_singleclass_best.csv",
                      "xgboost_preds_val_future_singleclass_1.csv",
                      "xgboost_preds_val_future_singleclass_2.csv",
                      "xgboost_preds_val_future_singleclass_3.csv",
                      "xgboost_preds_val_future_multiclass_best.csv",
                      "xgboost_preds_val_future_multiclass_1.csv",
                      "xgboost_preds_val_future_multiclass_2.csv",
                      "xgboost_preds_val_future_multiclass_3.csv")

blend.weights <- c(weight.single.best,rep(weight.single.other,3),
                  weight.multi.best,rep(weight.multi.best,3))

test.xgboost <- as.data.frame(fread(filenames.test[[1]]))
val.xgboost  <- as.data.frame(fread(filenames.val[[1]]))

pred.names <-names(test.xgboost[grepl("pred",names(test.xgboost))])
product.names <- gsub("\\_pred","",pred.names)

test.xgboost <- test.xgboost %>% 
  select(ncodpers,month.id,one_of(pred.names),one_of(product.names))

val.xgboost <- val.xgboost %>% 
  dplyr::select(ncodpers,month.id,one_of(pred.names),one_of(product.names))

meanval.test <- mean(data.matrix(test.xgboost[,names(test.xgboost) %in% pred.names]))
meanval.val  <- mean(data.matrix(val.xgboost[,names(val.xgboost) %in% pred.names]))

add.predictions <- function(current.predictions, new.filename, mean.val, blend.weight){
  preds <- as.data.frame(fread(new.filename))
  preds <- preds %>% 
    dplyr::select(ncodpers,month.id,one_of(pred.names),one_of(product.names))
  scale.factor <- blend.weight * mean.val / mean(data.matrix(preds[,names(preds) %in% pred.names]))
  preds[,names(preds) %in% pred.names] <- preds[,names(preds) %in% pred.names] * scale.factor
  current.predictions[,names(current.predictions) %in% pred.names] <-   current.predictions[,names(current.predictions) %in% pred.names] +
    preds[,names(preds) %in% pred.names]
  return(current.predictions)
}

for (file.num in 2:length(filenames.val)){
  print(paste("Adding model",filenames.val[[file.num]]))
  val.xgboost  <- add.predictions(val.xgboost, filenames.val[[file.num]],meanval.val,blend.weights[file.num])
  print(paste("Adding model",filenames.test[[file.num]]))
  test.xgboost <- add.predictions(test.xgboost, filenames.test[[file.num]],meanval.test,blend.weights[file.num])
}

write.csv(val.xgboost,"combined_preds_val.csv",row.names=FALSE)
write.csv(test.xgboost,"combined_preds_test.csv",row.names=FALSE)
