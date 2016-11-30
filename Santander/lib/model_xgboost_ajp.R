setwd("~/kaggle/competition-sandtander/")
library(tidyr)
library(xgboost)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(pROC)
library(lubridate)
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')

set.seed(1)
df   <- as.data.frame(fread("train_prepped.csv"))
test <- as.data.frame(fread("test_prepped.csv"))
labels <- names(df)[grepl(".*_target",names(df))]
purchase.w <- names(df)[grepl(".*.count",names(df))]
ownership.names <- names(df)[grepl("month\\_ago",names(df))]
numeric.cols <- c("age","renta","antiguedad",purchase.w,"total_products","num.transactions")
categorical.cols <- c("sexo","ind_nuevo","ind_empleado","segmento",
                      "conyuemp","nomprov","indfall","indext","indresi",ownership.names)


build.predictions.xgboost <- function(df, test, features, label, label.name,depth,eta){
  library(xgboost)
  # df:       training data
  # test:     the data to predict on
  # features: character vector of column names to use as features
  # label:    string representing which column to predict
  
  dtrain <- xgb.DMatrix(data = df, label=label)
  # test <- data.matrix(test[,names(test) %in% features])
  # test <- data.matrix(test)
  # dtest <- xgb.DMatrix(data = data.matrix(test[,names(test) %in% features]), label=data.matrix(test[[label]]))
  # model <- xgboost(data = dtrain,
  #                  max.depth = 10, 
  #                  eta = .05, nthread = 2,
  #                  nround = 100, 
  #                  objective = "binary:logistic", 
  #                  verbose =1 ,
  #                  print.every.n = 10)
  model <- xgboost(data = dtrain,
                   max.depth = depth, 
                   eta = eta, nthread = 4,
                   nround = 100, 
                   objective = "binary:logistic", 
                   verbose =1 ,
                   print.every.n = 10)
  print(xgb.importance(feature_names = colnames(df),model=model))
  predictions        <- list(predict(model,test))
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}



ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical.cols])), "dgCMatrix")
print("CLASS TYPE:")
print(class(test))
ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
ohe.test <- as(data.matrix(predict(ohe.test,test[,names(test) %in% categorical.cols])), "dgCMatrix")
train.labels        <- list()


for (label in labels){
  train.labels[[label]] <- as(data.matrix(df[[label]]),'dgCMatrix')
}
save.id       <- df$ncodpers
save.month.id <- df$month.id
save.id.test       <- test$ncodpers
save.month.id.test <- test$month.id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))
train.ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

test.save <- test
# val.save <- val
best.map <- 0
for (depth in c(5)){
  for (eta in c( 0.05)){
    test <- test.save
    # val <- val.save
predictions         <- list()
predictions_val     <- list()

# cycle through each label and 

label.count <- 1
for (label in labels){
  predictions_val <- c(predictions_val,build.predictions.xgboost(df[train.ind,],df[-train.ind,],c(numeric.cols,colnames(ohe)),train.labels[[label]][train.ind,1,drop=F],label,depth,eta) )
  accuracy <- mean(train.labels[[label]][-train.ind,1]==round(predictions_val[[label.count]]))
  print(sprintf("Accuracy for label %s = %f",label,accuracy))
  if (accuracy < 1){
  print(auc(roc(train.labels[[label]][-train.ind,1],predictions_val[[label.count]])))
  } else {
    print("auc perfect")
  }
    predictions <- c(predictions,build.predictions.xgboost(df,test,c(numeric.cols,colnames(ohe)),train.labels[[label]],label,depth,eta) )
  label.count <- label.count + 1
  
}
predictions <- as.data.table(predictions)
predictions_val <- as.data.table(predictions_val)
test        <- as.data.table(cbind(data.frame(data.matrix(test)),predictions))
val        <- as.data.table(cbind(data.frame(data.matrix(df[-train.ind,])),predictions_val))

test <- test[,grepl("ind_+.*_+ult",names(test)),with=FALSE]
test$ncodpers <- save.id.test
test$month.id <- save.month.id.test

val <- val[,grepl("ind_+.*_+ult",names(val)),with=FALSE]
# val <- cbind(list("ncodpers"=save.id.test[-train.ind],"month.id"=save.month.id.test[-train.ind]),val)
val$ncodpers <- save.id[-train.ind]
val$month.id <- save.month.id[-train.ind]
products <- gsub("_target","",labels)

full <- as.data.frame(fread("cleaned_train.csv"))
owned.products <- names(test)[grepl("ind_+.*_+ult",names(test)) & !(grepl("_pred",names(test)))]
if (length(owned.products)!=0){
test <- test[,!names(test) %in% owned.products, with=FALSE]
val  <- val[,!names(val) %in% owned.products, with=FALSE]
}
test <- merge(test %>%
                mutate(month.previous.id = month.id-1),
              full %>%
                dplyr::select(ind_ahor_fin_ult1:ind_recibo_ult1, month.id, ncodpers),by.x=c("ncodpers","month.previous.id"), by.y=c("ncodpers","month.id"),all.x=TRUE) 

val <- merge(val %>%
                mutate(month.previous.id = month.id-1),
              full %>%
                dplyr::select(ind_ahor_fin_ult1:ind_recibo_ult1, month.id, ncodpers),by.x=c("ncodpers","month.previous.id"), by.y=c("ncodpers","month.id"))

test[is.na(test)] <- 0
val[is.na(val)]   <- 0
test.recs <- get.recommendations(as.data.table(test),products)
val.recs  <- get.recommendations(as.data.table(val),products)
val$added_products <- val.recs$added_products

purchased <- as.data.frame(fread("purchased-products.csv"))
val <- val %>%
  merge(purchased,by=c("ncodpers","month.id"))
MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
print(paste("Validation MAP@7 = ",MAP))

if (MAP > best.map){
  best.map <- MAP
  out.recs <- test.recs
  best.depth <- depth
  best.eta <- eta
  
}
}
}
write.csv(out.recs,"recommendations.csv",row.names = FALSE)
