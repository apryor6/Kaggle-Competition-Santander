# This script is for tuning the blending weights used in combine-predictions-v2. Here only the validation dataset is used for speed
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')
source("project/Santander/lib/dataframe-correlation.R")
library(data.table)
library(dplyr)

base <- "/u/project/miao/apryor/ml/"

filenames.val <- list(paste(base,"xgboost_preds_val_future_singleclass_best.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_singleclass_1.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_singleclass_2.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_singleclass_3.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_multiclass_best.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_multiclass_1.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_multiclass_2.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_multiclass_3.csv",sep=""),
                      paste(base,"xgboost_preds_val_future_multiclass_4.csv",sep=""))

weight.single.best  <- 1
num.to.choose <- 5000

weight.single.best.vector  <- rep(1,num.to.choose)
weight.multi.best.vector   <- seq(0,1.5,0.05)
weight.single.other.vector <- seq(0,1.5,0.05)
weight.multi.other.vector  <- seq(0,1.5,0.05)


weight.multi.best.samples   <- sample(weight.multi.best.vector,num.to.choose,replace=TRUE)
weight.single.other.samples <- sample(weight.single.other.vector,num.to.choose,replace=TRUE)
weight.multi.other.samples  <- sample(weight.multi.other.vector,num.to.choose,replace=TRUE)

best.map <- 0
best.weight.multi.best   <- 0
best.weight.single.other <- 0
best.weight.multi.other  <- 0
for (run.num in 1:num.to.choose){
  print(paste("Run num =",run.num))
  weight.multi.best   <- weight.multi.best.samples[run.num]
  weight.single.other <- weight.single.other.samples[run.num]
  weight.multi.other  <- weight.multi.other.samples[run.num]
  blend.weights <- c(weight.single.best,rep(weight.single.other,3),
                     weight.multi.best,rep(weight.multi.other,3))
  
  val.blended  <- as.data.frame(fread(filenames.val[[1]]))
  
  pred.names <-names(val.blended[grepl("pred",names(val.blended))])
  product.names <- gsub("\\_pred","",pred.names)
  
  val.blended <- val.blended %>% 
    dplyr::select(ncodpers,month.id,one_of(pred.names),one_of(product.names))
  
  meanval.val  <- mean(data.matrix(val.blended[,names(val.blended) %in% pred.names]))
  
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
    # print(paste("Adding model",filenames.val[[file.num]]))
    val.blended  <- add.predictions(val.blended, filenames.val[[file.num]],meanval.val,blend.weights[file.num])
  }
  
  load("project/Santander/lib/products.Rdata")
  val.recs  <- get.recommendations(as.data.table(val.blended),products)
  val.blended$added_products <- val.recs$added_products
  purchased <- as.data.frame(fread("purchased-products.csv"))
  val.blended <- val.blended %>%
    merge(purchased,by=c("ncodpers","month.id"))
  MAP <- mapk(k=7,strsplit(val.blended$products, " "),strsplit(val.blended$added_products," "))
  print(paste("Validation MAP@7 = ",MAP))
  if (MAP > best.map){
    best.map <- MAP
    best.weight.multi.best   <- weight.multi.best
    best.weight.single.other <- weight.single.other
    best.weight.multi.other  <- weight.multi.other
    print(paste("New best MAP = " ,MAP))
    print(paste("best.weight.multi.best = ",best.weight.multi.best))
    print(paste("best.weight.single.other = ",best.weight.single.other))
    print(paste("best.weight.multi.other = ",best.weight.multi.other))
  }
}
