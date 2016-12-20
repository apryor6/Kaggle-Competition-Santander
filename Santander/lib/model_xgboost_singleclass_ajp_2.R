setwd("~/kaggle/competition-santander/")
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
use.many.seeds         <- TRUE
if (use.many.seeds){
  rand.seeds <- 121:130
} else{
  rand.seeds <- 1
}
# read data
# df   <- as.data.frame(fread("train_prepped.csv", stringsAsFactors = TRUE))
# test <- as.data.frame(fread("test_prepped.csv" , stringsAsFactors = TRUE))
load("data_prepped.RData")
use.extra.train.FLAG = TRUE
if (use.extra.train.FLAG){
  val.train <- rbind(val.train,extra.train.val)
  df       <- rbind(df,extra.train.test)
}
# df$ind_actividad_cliente <- sample(c(0,1),nrow(df),replace=TRUE)
# fraction.to.replace <- 0.50
# ind.to.replace <- sample(nrow(df),round(nrow(df))*fraction.to.replace)
# df$ind_actividad_cliente[ind.to.replace] <- df$ind_tjcr_fin_ult1_target[ind.to.replace]

# purchase.history <- fread("purchase-history.csv")
# df   <- merge(df,purchase.history,by=c("ncodpers","month.id"),sort=FALSE)
# test <- merge(test,purchase.history,by=c("ncodpers","month.id"),sort=FALSE)
# rm(purchase.history)

purchase.count <- fread("purchase-count.csv")
df   <- merge(df,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
test <- merge(test,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
val.train   <- merge(val.train,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
val.test <- merge(val.test,purchase.count,by=c("ncodpers","month.id"),sort=FALSE)
rm(purchase.count)
# this feature was to represent if somebody had recently moved, but no changes were made in first 6 months
# recently.moved <- fread("feature-recently-moved.csv")
# df   <- merge(df,recently.moved,by=c("ncodpers","month.id"),sort=FALSE)
# test <- merge(test,recently.moved,by=c("ncodpers","month.id"),sort=FALSE)
# rm(recently.moved)
# make sure the factor levels agree
factor.cols <- names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] <- factor(df[[col]],levels=union(levels(df[[col]]),levels(test[[col]])))
  val.train[[col]] <- factor(val.train[[col]],levels=union(levels(val.train[[col]]),levels(val.test[[col]])))
}

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight
labels               <- names(df)[grepl(".*_target",names(df)) & !grepl("ahor|aval",names(df))] # target values
purchase.w           <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
ownership.names      <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\.previous",names(df))] # various features indicating whether or not a product was owned X months ago
drop.names           <- names(df)[grepl("dropped",names(df))] # various features indicating whether or not a product was owned X months ago
add.names            <- names(df)[grepl("added",names(df))] # various features indicating whether or not a product was owned X months ago
num.added.names      <- names(df)[grepl("num\\.added",names(df))]  # total number of products added X months ago
num.purchases.names  <- names(df)[grepl("num\\.purchases",names(df))]  # total number of products added X months ago
total.products.names <- names(df)[grepl("total\\.products",names(df))]  # total number of products owned X months ago
owned.within.names   <- names(df)[grepl("owned\\.within",names(df))]  # whether or not each product was owned with X months
# numeric features to use
numeric.cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase.w)
                  # "total_products",
                  # "num.transactions",
                  # num.added.names,
                  # num.purchases.names)
# total.products.names)
# total.products.names)
#
# clust <- kmeans(rbind(df[,names(df) %in% numeric.cols],test[,names(test) %in% numeric.cols]),centers = 10)
# df[["clust"]] <- as.factor(clust$cluster[1:nrow(df)])
# test[["clust"]] <- as.factor(clust$cluster[(1+nrow(df)):length(clust$cluster)])
# categorical features. These will be one-hot encoded
categorical.cols <- c("sexo",
                      "ind_nuevo",
                      "ind_empleado",
                      "segmento",
                      "nomprov",
                      # "indext",
                      # "indresi",
                      # "indrel",
                      # "tiprel_1mes",
                      # ownership.names[grepl("1month",ownership.names)],
                      
                      ownership.names,
                      owned.within.names,
                      # "segmento.change",
                      "activity.index.change",
                      # "ind_actividad_cliente",
                      "month",
                      #                       "canal_entrada",
                      # ownership.names,
                      "birthday.month")
# added.products,
# dropped.products,
# "canal_entrada")
#canal entrada?


# one-hot encode the categorical features
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical.cols])), "dgCMatrix")
ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
ohe.test <- as(data.matrix(predict(ohe.test,test[,names(test) %in% categorical.cols])), "dgCMatrix")
ohe.val.train <- dummyVars(~.,data = val.train[,names(val.train) %in% categorical.cols])
ohe.val.train <- as(data.matrix(predict(ohe.val.train,val.train[,names(val.train) %in% categorical.cols])), "dgCMatrix")
ohe.val.test <- dummyVars(~.,data = val.test[,names(val.test) %in% categorical.cols])
ohe.val.test <- as(data.matrix(predict(ohe.val.test,val.test[,names(val.test) %in% categorical.cols])), "dgCMatrix")

train.labels        <- list()
train.labels.val        <- list()
# convert labels into XGBoost's sparse matrix representation
for (label in labels){
  train.labels[[label]]     <- as(data.matrix(df[[label]]),'dgCMatrix')
  train.labels.val[[label]] <- as(data.matrix(val.train[[label]]),'dgCMatrix')
}

# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save.id       <- df$ncodpers
save.month.id <- df$month.id
save.month    <- df$month
save.id.test       <- test$ncodpers
save.month.id.test <- test$month.id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))

save.id.val       <- val.train$ncodpers
save.month.id.val <- val.train$month.id
save.id.test.val       <- val.test$ncodpers
save.month.id.test.val <- val.test$month.id
save.month.val    <- val.train$month
val.train         <- cbind(ohe.val.train,data.matrix(val.train[,names(val.train) %in% numeric.cols]))
val.test       <- cbind(ohe.val.test,data.matrix(val.test[,names(val.test) %in% numeric.cols]))
set.seed(1)

# use a 75/25 train/test split so we can compute MAP@7 locally. The test set
# is predicted using a model trained on all of the training data
train.ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

# tuning hyperparameters to optimize MAP@7 must be done manually. I previously did 
# a grid search and these parameters were okay so I commented it out for now. You just 
# simply scan parameters and save the ones that gave you the best local MAP@7 on the validation data

test.save <- test
best.map <- 0
# for (depth in c(3,5,7,9,11,15)){
# for (eta in c(0.01,0.025, 0.05,0.1,0.25,0.5)){
depth <- 7
eta <- 0.05
test <- test.save
predictions         <- list()
predictions_val     <- list()
predictions_val_future     <- list()
# this function takes in training/testing data and returns predicted probabilities
build.predictions.xgboost <- function(df, test, label, label.name,depth,eta,weights,rand.seeds=0){
  for (rand.seed.num in 1:length(rand.seeds)){
    set.seed(rand.seeds[rand.seed.num])
    library(xgboost)
    # df:         training data
    # test:       the data to predict on
    # label:      vector containing the target label
    # label.name: name of the label
    # depth:      XGBoost max tree depth
    # eta:        XGBoost learning rate
    dtrain <- xgb.DMatrix(data = df, label=label,weight=weights)
    # model <- xgb.cv(data = dtrain,
    # max.depth = depth, 
    # eta = eta, nthread = 4,
    # nround = 100, 
    # objective = "binary:logistic", 
    # verbose =1 ,
    # print.every.n = 10,
    # nfold=5)
    model <- xgboost(data = dtrain,
                     max.depth = depth,
                     eta = eta, nthread = 4,
                     nround = 10, 
                     subsample=0.75,
                     # colsample_bytree=0.5,
                     objective = "binary:logistic", 
                     verbose =1 ,
                     print.every.n = 10)
    if (rand.seed.num == 1 ) { # initialize predictions on first time
      preds <- predict(model,test)
    } else {
      preds <- predict(model,test) + preds
    }
  }
  imp <- xgb.importance(feature_names = colnames(df),model=model)
  save(imp,file=paste("IMPORTANCE_",gsub("\\_target","",label.name),".RData",sep=""))
  print(imp)
  predictions        <- list(preds / length(rand.seeds))
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}

# loop over the labels and create predictions of the validation data and training data
# for each
label.count <- 1
for (label in labels){
  # the syntax for indexing train.labels is messy but functional
  # predictions_val <- c(predictions_val,build.predictions.xgboost(df[train.ind,],df[-train.ind,],train.labels[[label]][train.ind,1,drop=F],label,depth,eta) )
  # accuracy <- mean(train.labels[[label]][-train.ind,1]==round(predictions_val[[label.count]]))
  # print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  # if (accuracy < 1){ # perfect accuracy causes some error with pROC
  # print(pROC::auc(roc(train.labels[[label]][-train.ind,1],predictions_val[[label.count]])))
  # } else {
  # print("auc perfect")
  # }
  
  # now predict on the testing data
  downweight.factor <- 2
  predictions <- c(predictions,build.predictions.xgboost(df,test,train.labels[[label]],label,depth,eta,ifelse(save.month=="Jun",1,downweight.factor),rand.seeds) )
  predictions_val_future <- c(predictions_val_future,build.predictions.xgboost(val.train,val.test,train.labels.val[[label]],label,depth,eta,ifelse(save.month.val=="May",1,downweight.factor),rand.seeds) )
  label.count <- label.count + 1
  
}

# collect the results
predictions <- as.data.table(predictions)
# predictions_val <- as.data.table(predictions_val)
predictions_val_future <- as.data.table(predictions_val_future)
test        <- as.data.table(cbind(data.frame(data.matrix(test)),predictions))
# val        <- as.data.table(cbind(data.frame(data.matrix(df[-train.ind,])),predictions_val))
val_future        <- as.data.table(cbind(data.frame(data.matrix(val.test)),predictions_val_future))

# can drop some of the data at this point and put back the id's
test <- test[,grepl("ind_+.*_+ult",names(test)),with=FALSE]
test$ncodpers <- save.id.test
test$month.id <- save.month.id.test

# val <- val[,grepl("ind_+.*_+ult",names(val)),with=FALSE]
# val$ncodpers <- save.id[-train.ind]
# val$month.id <- save.month.id[-train.ind]

val_future <- val_future[,grepl("ind_+.*_+ult",names(val_future)),with=FALSE]
val_future$ncodpers <- save.id.test.val
val_future$month.id <- save.month.id.test.val

products <- gsub("_target","",labels)

# full <- as.data.frame(fread("cleaned_train.csv"))

# the features containing "1month_ago" will tell us whether or not a product is a new purchase in our predictions
owned.products <- names(test)[grepl("1month\\_ago",names(test)) & !(grepl("_pred",names(test)))]

# save the products for use in the recommendation script
save(products,file="project/Santander/lib/products.Rdata")

# put the predictions in the right format 
test <- test %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(test)[grepl("1month",names(test))] <- gsub("\\_1month\\_ago","",names(test)[grepl("1month",names(test))])
# val <- val %>%
#   select(ncodpers,month.id,contains("_pred"),contains("1month"))
# names(val)[grepl("1month",names(val))] <- gsub("\\_1month\\_ago","",names(val)[grepl("1month",names(val))])
val_future <- val_future %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(val_future)[grepl("1month",names(val_future))] <- gsub("\\_1month\\_ago","",names(val_future)[grepl("1month",names(val_future))])
# save the results


# test.recs <- get.recommendations(as.data.table(test),products)
# val.recs  <- get.recommendations(as.data.table(val),products)
# val$added_products <- val.recs$added_products
# 
# purchased <- as.data.frame(fread("purchased-products.csv"))
# val <- val %>%
#   merge(purchased,by=c("ncodpers","month.id"))
# MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
# print(paste("Validation MAP@7 = ",MAP))

val.recs.future  <- get.recommendations(as.data.table(val_future),products)
val_future$added_products <- val.recs.future$added_products

purchased <- as.data.frame(fread("purchased-products.csv"))
val_future <- val_future %>%
  merge(purchased,by=c("ncodpers","month.id"))
MAP <- mapk(k=7,strsplit(val_future$products, " "),strsplit(val_future$added_products," "))
print(paste("Validation future MAP@7 = ",MAP))

# if (MAP > best.map){
# best.map <- MAP
# out.recs <- test.recs
# best.depth <- depth
# best.eta <- eta

# }
# }
# }

write.csv(test,"/u/project/miao/apryor/ml/xgboost_preds_test_singleclass_2.csv",row.names = FALSE)
# write.csv(val,"xgboost_preds_val.csv",row.names = FALSE)
write.csv(val_future,"/u/project/miao/apryor/ml/xgboost_preds_val_future_singleclass_2.csv",row.names = FALSE)
# save.image(file="saved.workspace.RData")

# }
# write.csv(out.recs,"recommendations_xgboost.csv",row.names = FALSE)
