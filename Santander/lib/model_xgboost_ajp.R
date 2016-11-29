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
df   <- (fread("cleaned_train.csv"))
test <- as.data.frame(fread("cleaned_test.csv"))
drop.products <- c("ind_ahor_fin_ult1","ind_aval_fin_ult1")

df   <- df[,!names(df) %in% drop.products,with=FALSE]
test <- test[,!names(df) %in% drop.products]



df <- merge(df,df %>%
              dplyr::select(ind_cco_fin_ult1:ind_recibo_ult1, month.id, ncodpers),by.x=c("ncodpers","month.previous.id"), by.y=c("ncodpers","month.id"),all.x=TRUE)


df <- as.data.frame(df)
df$total_products <- rowSums(df[,names(df) %in% names(df)[grepl("ind.*\\.y",names(df))]],na.rm=TRUE)
test$total_products <- rowSums(test[,names(test) %in% names(test)[grepl("ind.*ult1",names(test))]],na.rm=TRUE)

df <- df %>%
  filter(fecha_dato%in%c("2015-06-28"))
# df <- df %>%
  # filter(fecha_dato%in%c("2016-05-28"))

purchase.frequencies <- fread("purchase.frequencies.csv")
purchase.frequencies.later.csv <- fread("purchase.frequencies.later.csv")

df   <- merge(df,purchase.frequencies,by=c("month.id","ncodpers"),all.x = TRUE)
test <- merge(test,purchase.frequencies.later.csv,by=c("month.id","ncodpers"), all.x=TRUE)
df[is.na(df)] <- 0
test[is.na(test)] <- 0

df$sexo[df$sexo=="UNKNOWN"] <- "V"
test$sexo[test$sexo=="UNKNOWN"] <- "V"

purchased <- as.data.frame(fread("purchased-products.csv"))
ids <- purchased$ncodpers[purchased$month.id == 6 & (purchased$products!="")]

df <- df[df$ncodpers %in% ids,]

# df <- df[sample(nrow(df),1e5),]
new.names <- names(df)
new.names[grepl("ind.*\\.y",new.names)] <- gsub("\\.y","",new.names[grepl("ind.*\\.y",new.names)])

new.names[grepl("ind.*\\.x",new.names)] <- gsub("\\.x","_target",new.names[grepl("ind.*\\.x",new.names)])

names(df) <- new.names

labels <- names(df)[grepl(".*_target",names(df))]
purchase.w <- names(df)[grepl(".*.count",names(df))]
products <- names(df)[grepl("ind_+.*_+ult",names(df)) & !grepl(".*_target|.count",names(df))]

# drop.labels <- c("ind_aval_fin_ult1_target","ind_ahor_fin_ult1_target")
# labels <- labels[!labels %in% drop.labels]
# numeric.cols <- c("age","renta","antiguedad","month")
numeric.cols <- c("age","renta","antiguedad",purchase.w,"total_products")
# numeric.cols <- c("age","renta","antiguedad","month",
#                   # gsub("_target","",labels)[1:7])
# categorical.cols <- c("sexo","ind_nuevo","ind_empleado","segmento",
#                       "conyuemp","nomprov","indfall","indext","indresi",
#                       "fecha_alta",products)
categorical.cols <- c("sexo","ind_nuevo","ind_empleado","segmento",
                      "conyuemp","nomprov","indfall","indext","indresi",products)

df$fecha_alta <- year(df$fecha_alta)
test$fecha_alta <- year(test$fecha_alta)

# categorical.cols <- c("sexo","ind_nuevo","ind_empleado","segmento",
#                       "conyuemp","nomprov","indfall","indext","indresi",
#                       "indrel_1mes","ult_fec_cli_1t","tiprel_1mes","indrel_1mes",
#                       "canal_entrada","indrel","conyuemp",
#                       products)

# df$month <- factor(month.abb[df$month],levels=month.abb)
# test$month <- factor(month.abb[test$month],levels=month.abb)
print(labels)

# for (label in labels){
#   base <- gsub("_target","",label)
#   vals <- (rowSums(df[,c(base,label)]))
#   print(table(vals))
#   
# }

test$ind_empleado[test$ind_empleado=="S"] <- "N" # Some rare value that was causing errors with factors later
char.cols <- names(test)[sapply(test,is.character)]
test[,char.cols] <- lapply(test[,char.cols], as.factor)

df$ind_empleado[df$ind_empleado=="S"] <- "N"
char.cols <- names(df)[sapply(df,is.character)]
df[,char.cols] <- lapply(df[,char.cols], as.factor)

factor.cols <- names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] <- factor(df[[col]],levels=levels(test[[col]]))
}
df$ult_fec_cli_1t[is.na(df$ult_fec_cli_1t)] <- "UNKNOWN"


build.predictions <- function(df, test, features, label){
  # df:       training data
  # test:     the data to predict on
  # features: character vector of column names to use as features
  # label:    string representing which column to predict
  
  
  # This function can be a major source of our tuning. As long as whatever models we build produce output in the same format as this then the rest of the code won't need to be changed much
  model      <- glm(as.formula(paste(label,paste(features,collapse=" + "),sep=" ~ ")),data=df)
  predictions_train <- predict(model,df[,names(df) %in% features],type="response")
  predictions       <- predict(model,test,type="response")
  print(sprintf("Accuracy for label %s = %f",label,mean(round(predictions_train)==df[[label]])))
  predictions <- list(predictions)
  names(predictions) <- paste(gsub("_target","",label),"_pred",sep="")
  return(predictions)
}

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


df <- df %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id)

test <- test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id) %>%
  as.data.frame()
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
