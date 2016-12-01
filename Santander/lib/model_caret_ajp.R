library(caret)
library(dplyr)
library(data.table)
library(doMC)
registerDoMC(cores = 4)
set.seed(1)

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
ownership.names <- names(df)[grepl("month\\_ago",names(df))] # various features indicating whether or not a product was owned X months ago

# numeric features to use
numeric.cols <- c("age",
                  "renta",
                  "antiguedad",
                  purchase.w,
                  "total_products",
                  "num.transactions")

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
                      ownership.names)
ohe <- dummyVars(~.,data = df[,names(df) %in% categorical.cols])
ohe <- as.data.frame(predict(ohe,df[,names(df) %in% categorical.cols]))
ohe.test <- dummyVars(~.,data = test[,names(test) %in% categorical.cols])
ohe.test <- as.data.frame(predict(ohe.test,test[,names(test) %in% categorical.cols]))
all.features <- c(numeric.cols, names(ohe))

# train.labels        <- list()

# for (label in labels){
  # train.labels[[label]] <- as.data.frame(df[[label]])
# }

# remember the id's for people and months for later since all that actually goes
# into the model is the raw feature data
save.id       <- df$ncodpers
save.month.id <- df$month.id
save.id.test       <- test$ncodpers
save.month.id.test <- test$month.id
df.labels  <- df[,names(df) %in% labels]
df.labels  <- data.frame(ifelse(df.labels==1,
                     "yes",
                     "no"))
df.labels[,] <- lapply(df.labels[,],as.factor)
df         <- cbind(ohe,data.matrix(df[,names(df) %in% numeric.cols]))
test       <- cbind(ohe.test,data.matrix(test[,names(test) %in% numeric.cols]))
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
  tc <- trainControl(method="cv",
                     number = 2,
                     repeats = 1,
                     verboseIter=TRUE,
                     classProbs=TRUE)
  # gbmGrid <-  expand.grid(interaction.depth = c(1),
                          # n.trees = c(100), 
                          # shrinkage = c(.001),
                          # n.minobsinnode = c(10)) 
  model <- train(df[,names(df) %in% features],
                 labels[[label.name]],
                 method=method,
                 trControl = tc,
                 # preProcess=c("center","scale"),
                 tuneGrid= data.frame(... ))
  # model <- train(df[,names(df) %in% features],
  #                label,
  #                method=method,
  #                # preProcess=c("center","scale"),
  #                tuneGrid=data.frame(n.trees=100,
  #                                    interaction.depth=2,
  #                                    shrinkage=0.01,
  #                                    n.minobsinnode=10))
  # model <- train(df[,names(df) %in% features],
  #                label,
  #                method=method,
  #                # preProcess=c("center","scale"),
  #                tuneGrid=data.frame(...))
  
  predictions        <- list(predict(model,test,type="prob")[["yes"]])
  names(predictions) <- paste(gsub("_target","",label.name),"_pred",sep="")
  return(predictions)
}

predictions         <- list()
predictions_val     <- list()

method <- "gbm"
params <- expand.grid(n.trees=c(500),
                     interaction.depth=c(10),
                     shrinkage=c(0.0025),
                     n.minobsinnode=c(10))
# loop over the labels and create predictions of the validation data and training data
# for each
label.count <- 1
for (label in labels){
  set.seed(1)
  print(paste("Label #",label.count))
  # the syntax for indexing train.labels is messy but functional
  predictions_val <- c(predictions_val,
                       build.predictions.caret(df[train.ind,],
                                               df[-train.ind,],
                                               df.labels[train.ind,],
                                               label,
                                               method,
                                               all.features,
                                               params))
  pred.labels <- ifelse(round(predictions_val[[label.count]])==1,
                              "yes",
                              "no")
  accuracy <- mean(df.labels[[label]][-train.ind]==pred.labels)
  print(sprintf("Accuracy for label %s = %f",label,accuracy)) # accuracy not super useful for this task
  if (accuracy < 1){ # perfect accuracy causes some error with pROC
    print(auc(roc(df.labels[[label]][-train.ind],predictions_val[[label.count]])))
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
                                           all.features,
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

