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
# start of commented section. I save the output so I don't have to rerun

# read data
df   <- as.data.frame(fread("train_prepped.csv", stringsAsFactors = TRUE))
test <- as.data.frame(fread("test_prepped.csv" , stringsAsFactors = TRUE))

# make sure the factor levels agree
factor.cols <- names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] <- factor(df[[col]],levels=levels(test[[col]]))
}

# there's a bunch of features related to the products, and thus they have similar
# names. Separate them out to keep things straight
labels <- names(df)[grepl(".*_target",names(df))] # target values
purchase.w <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
# ownership.names <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\.previous",names(df))] # various features indicating whether or not a product was owned X months ago
ownership.names <- names(df)[grepl("month\\_ago",names(df)) & !grepl("month\\.previous",names(df))] # various features indicating whether or not a product was owned X months ago

drop.names <- names(df)[grepl("dropped",names(df))] # various features indicating whether or not a product was owned X months ago
add.names <- names(df)[grepl("added",names(df))] # various features indicating whether or not a product was owned X months ago
last.owned.names <- names(df)[grepl("\\.last\\.owned",names(df))] # various features indicating whether or not a product was owned X months ago
print(paste("last.owned.names = ",last.owned.names))
purchased <- as.data.frame(fread("purchased-products.csv"))

products.df <- df %>%
  select(ncodpers,month.id) %>%
  merge(purchased,by=c("ncodpers","month.id"),sort=FALSE) %>%
  filter(products!="")
set.seed(1)
products.df$products <- sapply(strsplit(products.df$products, " "), function(x) sample(x,1))
products.df$products <- factor(products.df$products,levels=gsub("\\_target","",labels))
product.names.save<-gsub("\\_target","",labels)
# product.names.save   <- levels(products.df$products)
# product.names.save   <- unique(products.df$products)

products.df$products <- as.integer(products.df$products)-1
# convert labels into XGBoost's sparse matrix representation
train.labels <- list()
train.labels[["products"]] <- as(data.matrix(products.df[["products"]]),'dgCMatrix')



# numeric features to use
numeric.cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase.w,
                  "total_products",
                  "num.transactions",
                  last.owned.names)
# categorical features. These will be one-hot encoded
categorical.cols <- c("sexo",
                      "ind_nuevo",
                      "ind_empleado",
                      "segmento",
                      "conyuemp",
                      "nomprov",
                      "indfall",
                      "indext",
                      "indresi",
                      ownership.names,
                      drop.names,
                      add.names)

# categorical.cols <- c("sexo",
#                       "ind_nuevo",
#                       "ind_empleado",
#                       "segmento",
#                       "conyuemp",
#                       "nomprov",
#                       "indfall",
#                       "indext",
#                       "indresi",
#                       ownership.names)


# categorical.cols <- c("sexo",
                      # "ind_nuevo",
                      # "ind_empleado",
                      # "segmento",
                      # "conyuemp",
                      # "nomprov",
                      # "indfall",
                      # "indext",
                      # "indresi")
#

# one-hot encode the categorical features
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
ohe <- as(data.matrix(predict(ohe,df[,names(df) %in% categorical.cols])), "dgCMatrix")
ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
ohe.test <- as(data.matrix(predict(ohe.test,test[,names(test) %in% categorical.cols])), "dgCMatrix")


# remember the id's for people and months for later since all that actually goes
# into xgboost is the raw feature data
save.id       <- df$ncodpers
save.month.id <- df$month.id
save.id.test       <- test$ncodpers
save.month.id.test <- test$month.id
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))
save.image(file="shortcut.RData")
### end
load("shortcut.RData")
set.seed(1)




# use a 75/25 train/test split so we can compute MAP@7 locally. The test set
# is predicted using a model trained on all of the training data
train.ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

# tuning hyperparameters to optimize MAP@7 must be done manually. I previously did 
# a grid search and these parameters were okay so I commented it out for now. You just 
# simply scan parameters and save the ones that gave you the best local MAP@7 on the validation data

# test.save <- test
best.map <- 0
# for (depth in c(5)){
# for (eta in c( 0.05)){
depth <- 7
eta <- 0.025
# test <- test.save
predictions         <- list()
predictions_val     <- list()

# this function takes in training/testing data and returns predicted probabilities
build.predictions.xgboost <- function(df, test, label, label.name,depth,eta){
  library(xgboost)
  # df:         training data
  # test:       the data to predict on
  # label:      vector containing the target label
  # label.name: name of the label
  # depth:      XGBoost max tree depth
  # eta:        XGBoost learning rate
  
  dtrain <- xgb.DMatrix(data = df, label=label)
  model <- xgboost(data = dtrain,
                   max.depth = depth, 
                   eta = eta, nthread = 2,
                   nround = 1000, 
                   objective = "multi:softprob", 
                   num_class=22, #hardcoded!
                   verbose =1 ,
                   print.every.n = 10)
  # imp <- xgb.importance(feature_names = colnames(df),model=model)
  # save(imp,file=paste("IMPORTANCE_multiclass_",gsub("\\_target","",label.name),".RData",sep=""))
  # print(imp)
  predictions        <- list(predict(model,test))
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}

# loop over the labels and create predictions of the validation data and training data
# for each
label.count <- 1
for (label in c("products")){
  print("training...")
  # the syntax for indexing train.labels is messy but functional
  predictions_val <- c(predictions_val,build.predictions.xgboost(df[train.ind,],df[-train.ind,],train.labels[[label]][train.ind,1,drop=F],label,depth,eta) )
  # accuracy <- mean(train.labels[[label]][-train.ind,1]==round(predictions_val[[label.count]]))
  # print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  # if (accuracy < 1){ # perfect accuracy causes some error with pROC
    # print(pROC::auc(roc(train.labels[[label]][-train.ind,1],predictions_val[[label.count]])))
  # } else {
    # print("auc perfect")
  # }
  print("predicting...")
  # now predict on the testing data
  predictions <- c(predictions,build.predictions.xgboost(df,test,train.labels[[label]],label,depth,eta) )
  label.count <- label.count + 1
  
}
#tmp_debugginRData orginates here
predictions[[1]]     <- matrix(predictions[[1]],nrow=nrow(test),byrow = TRUE)
predictions_val[[1]] <- matrix(predictions_val[[1]],nrow=(nrow(df)-length(train.ind)),byrow = TRUE)
colnames(predictions[[1]]) <- product.names.save
colnames(predictions_val[[1]]) <- product.names.save

# collect the results
predictions <- as.data.table(as.data.frame(predictions))
predictions_val <- as.data.table(as.data.frame(predictions_val))
test        <- as.data.table(cbind(data.frame(data.matrix(test)),predictions))
val        <- as.data.table(cbind(data.frame(data.matrix(df[-train.ind,])),predictions_val))

# can drop some of the data at this point and put back the id's
test <- test[,grepl("ind_+.*_+ult",names(test)),with=FALSE]
test$ncodpers <- save.id.test
test$month.id <- save.month.id.test

val <- val[,grepl("ind_+.*_+ult",names(val)),with=FALSE]
val$ncodpers <- save.id[-train.ind]
val$month.id <- save.month.id[-train.ind]
products <- gsub("_target","",labels)

# full <- as.data.frame(fread("cleaned_train.csv"))

# the features containing "1month_ago" will tell us whether or not a product is a new purchase in our predictions
owned.products <- names(test)[grepl("1month\\_ago",names(test)) & !(grepl("_pred",names(test)))]

# save the products for use in the recommendation script
# save(products,file="project/Santander/lib/products.Rdata")
old.names <- names(test)
old.names[grepl("products\\_pred",old.names)] <- paste(gsub("products\\_pred\\.","",old.names[grepl("products\\_pred\\.",old.names)]),"_pred",sep="")
names(test) <- old.names

old.names <- names(val)
old.names[grepl("products\\_pred",old.names)] <- paste(gsub("products\\_pred\\.","",old.names[grepl("products\\_pred\\.",old.names)]),"_pred",sep="")
names(val) <- old.names


# put the predictions in the right format 
test <- test %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(test)[grepl("1month",names(test))] <- gsub("\\_1month\\_ago","",names(test)[grepl("1month",names(test))])
val <- val %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(val)[grepl("1month",names(val))] <- gsub("\\_1month\\_ago","",names(val)[grepl("1month",names(val))])

# save the results
write.csv(test,"xgboost_preds_test_multiclass.csv")
write.csv(val,"xgboost_preds_val_multiclass.csv")


# test.recs <- get.recommendations(as.data.table(test),products)
# val.recs  <- get.recommendations(as.data.table(val),products)
# val$added_products <- val.recs$added_products

# purchased <- as.data.frame(fread("purchased-products.csv"))
# val <- val %>%
# merge(purchased,by=c("ncodpers","month.id"))
# MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
# print(paste("Validation MAP@7 = ",MAP))

# if (MAP > best.map){
# best.map <- MAP
# out.recs <- test.recs
# best.depth <- depth
# best.eta <- eta
# }
# }
# }
# write.csv(out.recs,"recommendations_xgboost.csv",row.names = FALSE)
