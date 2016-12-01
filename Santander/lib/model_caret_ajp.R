library(caret)
library(dplyr)
library(data.table)
library(doMC)
registerDoMC(cores = 4)
set.seed(1)
NUM.FEATURES.TO.USE <- 50 # number of top features by XGBoost to use in each model

# This script can build many different models by adjusting MODEL.NUMBER. 
# A series of model names and associated hyperparameters are accessed in a list
# with this parameter, and the rest of the script is generic. With slight modification
# this parameter can be taken as an input which would allow distribution of 
# model training across many nodes in a cluster, etc. Encapsulation makes life easier
MODEL.NUMBER <- 3 # numeric indicator of which caret L1 model to build.

params.list <- list()
params.list[["gbm"]] <- expand.grid(n.trees=c(500),
                                               interaction.depth=c(5),
                                               shrinkage=c(5*0.0025),
                                               n.minobsinnode=c(10))

params.list[["glmnet"]] <- expand.grid(alpha=c(0.1),
                                               lambda=c(1))

params.list[["svmLinear"]] <- expand.grid(C=c(0.1))

method.list <- as.list(names(params.list))

method <- method.list[[MODEL.NUMBER]]
params <- params.list[[method]] 

# read data
# This file "caret_data_prepped" contains the results of the commented out section 
# which does a little preprocessing specific to caret but takes some time (mostly the
# one-hot encoding bit). So I use this shortcut
load("caret_data_prepped")
# df   <- as.data.frame(fread("train_prepped_caret.csv", stringsAsFactors = TRUE))
# test <- as.data.frame(fread("test_prepped_caret.csv" , stringsAsFactors = TRUE))
# 
# # make sure the factor levels agree
# factor.cols <- names(test)[sapply(test,is.factor)]
# for (col in factor.cols){
#   df[[col]] <- factor(df[[col]],levels=levels(test[[col]]))
# }
# 
# 
# # there's a bunch of features related to the products, and thus they have similar
# # names. Separate them out to keep things straight
# labels <- names(df)[grepl(".*_target",names(df))] # target values
# purchase.w <- names(df)[grepl(".*.count",names(df))] # number of times a product has been bought in the past 5 months
# ownership.names <- names(df)[grepl("month\\_ago",names(df))] # various features indicating whether or not a product was owned X months ago
# 
# # numeric features that were used in the XGBoost model. This will be trimmed down
# # for each actual label based on the feature importance determined by XGBoost
# numeric.cols <- c("age",
#                   "renta",
#                   "antiguedad",
#                   purchase.w,
#                   "total_products",
#                   "num.transactions")
# 
# # categorical features that were one-hot encoded in the XGBoost model. This will be trimmed down
# # for each actual label based on the feature importance determined by XGBoost
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
# ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
# ohe <- as.data.frame(predict(ohe,df[,names(df) %in% categorical.cols]))
# ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
# ohe.test <- as.data.frame(predict(ohe.test,test[,names(test) %in% categorical.cols]))
# all.features <- c(numeric.cols, names(ohe))
# 
# # remember the id's for people and months for later since all that actually goes
# # into the model is the raw feature data
# save.id       <- df$ncodpers
# save.month.id <- df$month.id
# save.id.test       <- test$ncodpers
# save.month.id.test <- test$month.id
# df.labels  <- df[,names(df) %in% labels]
# df.labels  <- data.frame(ifelse(df.labels==1,
#                      "yes",
#                      "no"))
# df.labels[,] <- lapply(df.labels[,],as.factor)
# df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
# test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))


set.seed(1)

# use a 75/25 train/test split so we can compute MAP@7 locally. The test set
# is predicted using a model trained on all of the training data
train.ind  <- createDataPartition(1:nrow(df),p=0.75)[[1]]

# this function takes in training/testing data and returns predicted probabilities
build.predictions.caret <- function(df, test, labels, label.name, method, features, ...){
  # df:         training data
  # test:       the data to predict on
  # label:      target labels
  # label.name: name of the target label to use
  # method:     caret model to use
  # features:   which features to use
  # ...:        list of parameters to pass to tuneGrid
  
  # the hyperparameters are optimized using a grid search and maximizing
  # the validation set's MAP@7, so there's no point in doing repeated CV
  # and I'll just turn it off
  tc <- trainControl(method="none",
                     number = 2,
                     repeats = 1,
                     verboseIter=TRUE,
                     classProbs=TRUE)
 
  model <- train(df[,names(df) %in% features],
                 labels[[label.name]],
                 method=method,
                 trControl=tc,
                 # preProcess=c("center","scale"),
                 tuneGrid=data.frame(...))

  predictions        <- list(predict(model,test[names(test) %in% features],type="prob")[["yes"]])
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}

predictions         <- list()
predictions_val     <- list()


# loop over the labels and create predictions of the validation data and training data
# for each
label.count <- 1
for (label in labels){
  importance.file <- paste("IMPORTANCE_",gsub("\\_target","",label),".RData",sep="")
  load(importance.file)
  features.to.use <- imp$Feature[1:min(NUM.FEATURES.TO.USE,nrow(imp))]
  # features.to.use <- all.features[all.features %in% features.to.use]
  # features.to.use <- all.features[1:100]
  
  set.seed(1)
  print(paste("Label #",label.count))
  predictions_val <- c(predictions_val,
                       build.predictions.caret(df[train.ind,],
                                               df[-train.ind,],
                                               df.labels[train.ind,],
                                               label,
                                               method,
                                               features.to.use,
                                               params))
  pred.labels <- ifelse(round(predictions_val[[label.count]])==1,
                              "yes",
                              "no")
  accuracy <- mean(df.labels[[label]][-train.ind]==pred.labels)
  print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  if (accuracy < 1){ # perfect accuracy causes some error with pROC
    print(pROC::auc(roc(df.labels[[label]][-train.ind],predictions_val[[label.count]])))
  } else {
    print("auc perfect")
  }
  
  # now predict on the testing data
  predictions <- c(predictions,
                   build.predictions.caret(df,
                                           test,
                                           df.labels,
                                           label,
                                           method,
                                           features.to.use,
                                           params) )
  label.count <- label.count + 1
}

# collect the results
predictions <- as.data.table(predictions)
predictions_val <- as.data.table(predictions_val)
test        <- as.data.table(cbind(test,data.frame(predictions)))
val        <- as.data.table(cbind(df[-train.ind,],data.frame(predictions_val)))

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

# put the predictions in the right format 
test <- test %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(test)[grepl("1month",names(test))] <- gsub("\\_1month\\_ago","",names(test)[grepl("1month",names(test))])
val <- val %>%
  select(ncodpers,month.id,contains("_pred"),contains("1month"))
names(val)[grepl("1month",names(val))] <- gsub("\\_1month\\_ago","",names(val)[grepl("1month",names(val))])

# save the results
write.csv(test,"caret_preds_test.csv")
write.csv(val,"caret_preds_val.csv")

