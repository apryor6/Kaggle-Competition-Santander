#setwd("~/kaggle/competition-santander/")
library(tidyr)
library(xgboost)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(caret)
library(pROC)
library(lubridate)
library(fasttime)
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')
source("project/Santander/lib/months-since-owned.R")

set.seed(1)
# Train on month 5 and 11 and validate on 17 for CV data then
# train on month 6 and 12 and predict on test. The second months are separated
# into a separate variable so I can turn on/off using them
val.train.month <- 5
val.test.month  <- 17
train.month     <- 6
extra.train.months.val <- c(11)
extra.train.months.test <- c(12)

months.to.keep  <- c(val.train.month,val.test.month,train.month,extra.train.months.val,extra.train.months.test)
df   <- fread("cleaned_train.csv")
test <- fread("cleaned_test.csv")

# add activity index previous month
recent.activity.index <- merge(rbind(df[,.(ncodpers,month.id,ind_actividad_cliente,
                                     segmento)],
                                     test[,.(ncodpers,month.id,ind_actividad_cliente,
                                           segmento)]),
                               df[,.(ncodpers,month.id=month.id+1,
                                     old.ind_actividad_cliente=ind_actividad_cliente,
                                     old.segmento=segmento)],
                               by=c("ncodpers","month.id"),
                               sort=FALSE)
                               # all.x=TRUE) # might not want all.x here, means people that weren't customers last month will be considered to change activity
recent.activity.index[,activity.index.change:=ind_actividad_cliente-old.ind_actividad_cliente]
recent.activity.index[,segmento.change:=as.integer(segmento!=old.segmento)]
df   <- merge(df,recent.activity.index[,.(ncodpers,
                                          month.id,
                                          old.ind_actividad_cliente,
                                          activity.index.change,
                                          old.segmento,
                                          segmento.change)],
              by=c("ncodpers","month.id"),all.x=TRUE)

test <- merge(test,recent.activity.index[,.(ncodpers,
                                            month.id,
                                            old.ind_actividad_cliente,
                                            activity.index.change,
                                            old.segmento,
                                            segmento.change)],
              by=c("ncodpers","month.id"),all.x=TRUE)

df$old.segmento[is.na(df$old.segmento)] <- df$segmento[is.na(df$old.segmento)] 
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- df$old.ind_actividad_cliente[is.na(df$ind_actividad_cliente)] 

df[is.na(df)] <- 0

products <- names(df)[grepl("ind_+.*_+ult",names(df))]

# create a data frame with just the product ownership variables so we can create lag ownership features
products.owned <- df %>%
  select(ncodpers,month.id,one_of(products)) %>%
  as.data.table()

df   <- as.data.table(df)
test <- as.data.table(test)
original.month.id <- products.owned$month.id
df <- df[month.id %in% months.to.keep,]


test <- test[,!names(test) %in% products,with=FALSE] #lazy, but I'm removing product ownership because it is about to be readded month by month

# create features indicating whether or not a product was owned in each of the past
# X months. for each lag, match the month with the earlier one and through some name manipulation
# extract whether the product was owned or not
for (month.ago in 1:11){
  print(paste("Collecting data on product ownership",month.ago,"months ago..."))
  products.owned[,month.id:=original.month.id+month.ago]
  df <- merge(df,products.owned,by=c("ncodpers","month.id"),all.x=TRUE)
  change.names <- names(df)[grepl("\\.y",names(df))]
  new.names <- gsub("\\.y",paste("_",month.ago,"month_ago",sep=""),change.names)
  names(df)[grepl("\\.y",names(df))] <- new.names
  
  #I'm being lazy here...
  change.names <- names(df)[grepl("\\.x",names(df))]
  new.names <- gsub("\\.x","",change.names)
  names(df)[grepl("\\.x",names(df))] <- new.names
  
  
  test <- merge(test,products.owned,by=c("ncodpers","month.id"),all.x=TRUE)
  
  change.names <- names(test)[grepl("\\.y",names(test))]
  new.names <- gsub("\\.y",paste("_",month.ago,"month_ago",sep=""),change.names)
  names(test)[grepl("\\.y",names(test))] <- new.names
  
  change.names <- names(test)[grepl("\\.x",names(test))]
  new.names <- gsub("\\.x","",change.names)
  names(test)[grepl("\\.x",names(test))] <- new.names
  
}
names(test)[names(test) %in% products] <- paste(names(test)[names(test) %in% products],"_1month_ago",sep="")

# there will be NA values where there isn't a match to the left side since we used all.x=TRUE, assume those correspond
# to products that were not owned
df[is.na(df)] <- 0
test[is.na(test)] <- 0

# get the number of months since each product was owned
df <- months.since.owned(df,products,12)
test <- months.since.owned(test,products,12)
df <- as.data.frame(df)
test <- as.data.frame(test)


# compute total number of products owned previous month
df$total_products <- rowSums(df[,names(df) %in% names(df)[grepl("ind.*1month\\_ago",names(df))]],na.rm=TRUE)
test$total_products <- rowSums(test[,names(test) %in% names(test)[grepl("ind.*1month\\_ago",names(test))]],na.rm=TRUE)

# save the month id for use creating window ownership features
products.owned$month.id <- original.month.id

# windows of product ownership. For each window size look back at previous months and see if the product was 
# ever owned. I do this by adding the value of the ownership variable X months ago for X = 1:window.size
# then converting to a binary indicator if the value is positive (meaning it was owned at least once)
 for (product in products){
   for (window.size in 2:6){
     print(paste("Getting ownership for",product,"within last",window.size,"months"))
     colname <- paste(product,".owned.within.",window.size,"months",sep="")
     df[[colname]]   <- 0
     test[[colname]] <- 0
     for (month.ago in 1:window.size){
      current.col     <- paste(product,"_",month.ago,"month_ago",sep="")
      df[[colname]]   <- df[[colname]]  + df[[current.col]]
      test[[colname]] <- test[[colname]]  + test[[current.col]]
     }
     df[[colname]]   <- as.integer(df[[colname]] > 0)
     test[[colname]] <- as.integer(test[[colname]] > 0)
   }
 }

# add in purchase frequency feature for each product
purchase.frequencies <- fread("purchase.frequencies.csv")

df   <- merge(df,purchase.frequencies,by=c("month.id","ncodpers"),all.x = TRUE)
test <- merge(test,purchase.frequencies,by=c("month.id","ncodpers"), all.x=TRUE)
df[is.na(df)] <- 0
test[is.na(test)] <- 0

# fix some rare value that was causing an error
df$sexo[df$sexo=="UNKNOWN"] <- "V"
test$sexo[test$sexo=="UNKNOWN"] <- "V"

# append "_target" so I can keep straight which are the target variables and which indicate ownership as a feature
new.names <- names(df)
new.names[new.names %in% products] <- paste(new.names[new.names %in% products],"_target",sep="")
names(df) <- new.names

labels <- names(df)[grepl(".*_target",names(df))]
purchase.w <- names(df)[grepl(".*.count",names(df))]
# products <- names(df)[grepl("ind_+.*_+ult",names(df)) & !grepl(".*_target|.count|month\\_ago",names(df))]
ownership.names <- names(df)[grepl("month\\_ago",names(df))]


test$ind_empleado[test$ind_empleado=="S"] <- "N" # Some rare value that was causing errors with factors later
char.cols <- names(test)[sapply(test,is.character)]
test[,char.cols] <- lapply(test[,char.cols], as.factor)

df$ind_empleado[df$ind_empleado=="S"] <- "N"
char.cols <- names(df)[sapply(df,is.character)]
df[,char.cols] <- lapply(df[,char.cols], as.factor)

# force the factor levels to be the same 
factor.cols <- names(test)[sapply(test,is.factor)]
for (col in factor.cols){
  df[[col]] <- factor(df[[col]],levels=levels(test[[col]]))
}
df$ult_fec_cli_1t[is.na(df$ult_fec_cli_1t)] <- "UNKNOWN"

# only keep entries where customers purchased products and the month matches one of our sets
purchased <- as.data.frame(fread("purchased-products.csv"))
ids.val.train   <- purchased$ncodpers[purchased$month.id %in% val.train.month & (purchased$products!="")]
ids.val.test    <- purchased$ncodpers[purchased$month.id %in% val.test.month & (purchased$products!="")]
ids.train       <- purchased$ncodpers[purchased$month.id %in% train.month & (purchased$products!="")]

extra.train.ids.val <- purchased$ncodpers[purchased$month.id %in% extra.train.months.val & (purchased$products!="")]
extra.train.ids.test <- purchased$ncodpers[purchased$month.id %in% extra.train.months.test & (purchased$products!="")]

# convert the birthday month feature to a named factor
df$birthday.month   <- factor(month.abb[df$birthday.month],levels=month.abb)
test$birthday.month <- factor(month.abb[test$birthday.month],levels=month.abb)

df$month   <- factor(month.abb[df$month],levels=month.abb)
test$month <- factor(month.abb[test$month],levels=month.abb)

# discard some columns that are no longer useful
df <- select(df,-fecha_alta,-fecha_dato,-month.previous.id)

# separate the data into the various parts
extra.train.val <- df %>% 
  filter(ncodpers %in% extra.train.ids.val & month.id %in% extra.train.months.val)

extra.train.test <- df %>% 
  filter(ncodpers %in% extra.train.ids.test & month.id %in% extra.train.months.test)

val.train <- df %>% 
  filter(ncodpers %in% ids.val.train & month.id %in% val.train.month)

val.test <- df %>% 
  filter(ncodpers %in% ids.val.test & month.id %in% val.test.month) 

df <- df %>% 
  filter(ncodpers %in% ids.train & month.id %in% train.month) 

test <- test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id) 

# save as binary for faster loading
save(df,test,val.train,val.test,extra.train.val,extra.train.test,file="data_prepped.RData")

