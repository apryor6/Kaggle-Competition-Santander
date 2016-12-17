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
library(fasttime)
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')

set.seed(1)
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
# recent.activity.index[is.na(recent.activity.index)] <- 0
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
# test[is.na(test)] <- 0

products <- names(df)[grepl("ind_+.*_+ult",names(df))]

# we are training only on june 2015, so there is 5 months of history before that.
# so I will consider only the 5 months before the testing date for creating features
products.owned <- df %>%
  # filter(month.id <= 6 | month.id >=12) %>%
  select(ncodpers,month.id,one_of(products)) %>%
  as.data.table()

df   <- as.data.table(df)
test <- as.data.table(test)
original.month.id <- products.owned$month.id
df <- df[month.id %in% months.to.keep,]


# test <- as.data.table(test)
test <- test[,!names(test) %in% products,with=FALSE] #lazy, but I'm removing product ownership because it is about to be readded month by month
# original.month.id <- products.owned$month.id
# df <- df[month.id=6,] # only train June 2015

# create features indicating whether or not a product was owned in each of the past
# 5 months. for each lag, match the month with the earlier one and through some name manipulation
# extract whether the product was owned or not
for (month.ago in 1:12){
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
# rm(list=c("products.owned","original.month.id"))

df[is.na(df)] <- 0
test[is.na(test)] <- 0

df <- as.data.frame(df)
test <- as.data.frame(test)


# compute total number of products owned previous month
df$total_products <- rowSums(df[,names(df) %in% names(df)[grepl("ind.*1month\\_ago",names(df))]],na.rm=TRUE)
test$total_products <- rowSums(test[,names(test) %in% names(test)[grepl("ind.*1month\\_ago",names(test))]],na.rm=TRUE)


#### try inserting here instead
products.owned$month.id <- original.month.id
# 
# df <- as.data.table(df)
# test <- as.data.table(test)
# products.owned[,month.previous.id:=month.id-1]
# dropped.products <- merge(products.owned,products.owned,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE,sort=FALSE)
# dropped.products[is.na(dropped.products)] <- 0
# added.products <- dropped.products
# for (product in products){
#   print(paste("Getting drop history for",product))
#   colx <- paste(product,".x",sep="")
#   coly <- paste(product,".y",sep="")
#   diff <- dropped.products[,(get(colx)-get(coly))]
#   dropped.products[,dropped:=ifelse(diff<0,1,0)]
# 
#   diff.added <- added.products[,(get(colx)-get(coly))]
#   added.products[,added:=ifelse(diff.added>0,1,0)]
# 
#   print(paste("Num dropped",sum(dropped.products$dropped)))
#   print(paste("Num added",sum(added.products$added)))
# 
#   for(month.ago in 1:5){
#     colname <- paste(product,"_dropped_",month.ago,"months_ago",sep="")
#     tmp <- merge(df[,.(ncodpers,month.id=month.id-month.ago)],dropped.products[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),all.x=TRUE,sort=FALSE)
#     # tmp <- merge(df[,.(ncodpers,month.id=month.id-1)],tmp[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),sort=FALSE)
#     df[[colname]] <- tmp$dropped
# 
#     tmp <- merge(test[,.(ncodpers,month.id=month.id-month.ago)],dropped.products[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),all.x=TRUE,sort=FALSE)
#     # tmp <- merge(test[,.(ncodpers,month.id=month.id-1)],tmp[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),sort=FALSE)
#     test[[colname]] <- tmp$dropped
# 
# 
#     colname <- paste(product,"_added_",month.ago,"months_ago",sep="")
#     tmp <- merge(df[,.(ncodpers,month.id=month.id-month.ago)],added.products[,.(ncodpers,month.id,added)],by=c("ncodpers","month.id"),all.x=TRUE,sort=FALSE)
#     # tmp <- merge(df[,.(ncodpers,month.id=month.id-1)],tmp[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),sort=FALSE)
#     df[[colname]] <- tmp$added
# 
#     tmp <- merge(test[,.(ncodpers,month.id=month.id-month.ago)],added.products[,.(ncodpers,month.id,added)],by=c("ncodpers","month.id"),all.x=TRUE,sort=FALSE)
#     # tmp <- merge(test[,.(ncodpers,month.id=month.id-1)],tmp[,.(ncodpers,month.id,dropped)],by=c("ncodpers","month.id"),sort=FALSE)
#     test[[colname]] <- tmp$added
# 
#   }
# }
# df[is.na(df)] <- 0
# test[is.na(test)] <- 0
# df <- as.data.frame(df)
# test <- as.data.frame(test)
# # 
# 
# 
# 
# windows of product ownership
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



## need to fix this
# df <- unique(df)
# test <- unique(test)

purchase.frequencies <- fread("purchase.frequencies.csv")
purchase.frequencies.later.csv <- fread("purchase.frequencies.later.csv")

df   <- merge(df,purchase.frequencies,by=c("month.id","ncodpers"),all.x = TRUE)
test <- merge(test,purchase.frequencies.later.csv,by=c("month.id","ncodpers"), all.x=TRUE)
df[is.na(df)] <- 0
test[is.na(test)] <- 0

df$sexo[df$sexo=="UNKNOWN"] <- "V"
test$sexo[test$sexo=="UNKNOWN"] <- "V"




new.names <- names(df)
new.names[new.names %in% products] <- paste(new.names[new.names %in% products],"_target",sep="")
names(df) <- new.names

labels <- names(df)[grepl(".*_target",names(df))]
purchase.w <- names(df)[grepl(".*.count",names(df))]
# products <- names(df)[grepl("ind_+.*_+ult",names(df)) & !grepl(".*_target|.count|month\\_ago",names(df))]
ownership.names <- names(df)[grepl("month\\_ago",names(df))]
numeric.cols <- c("age","renta","antiguedad",purchase.w,"total_products","num.transactions")


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

purchased <- as.data.frame(fread("purchased-products.csv"))
ids.val.train   <- purchased$ncodpers[purchased$month.id %in% val.train.month & (purchased$products!="")]
ids.val.test    <- purchased$ncodpers[purchased$month.id %in% val.test.month & (purchased$products!="")]
ids.train       <- purchased$ncodpers[purchased$month.id %in% train.month & (purchased$products!="")]
# extra.train.ids <- purchased$ncodpers[purchased$month.id %in% extra.train.months & (purchased$products!="")]
# extra.train.ids.val <- intersect(purchased$ncodpers[purchased$month.id %in% extra.train.months.val & (purchased$products!="")],
                             # df$ncodpers[df$segmento.change==1 | df$activity.index.change==1])
# extra.train.ids.test <- intersect(purchased$ncodpers[purchased$month.id %in% extra.train.months.test & (purchased$products!="")],
                                 # df$ncodpers[df$segmento.change==1 | df$activity.index.change==1])

extra.train.ids.val <- purchased$ncodpers[purchased$month.id %in% extra.train.months.val & (purchased$products!="")]
extra.train.ids.test <- purchased$ncodpers[purchased$month.id %in% extra.train.months.test & (purchased$products!="")]
extra.train.ids.may <-  purchased$ncodpers[(purchased$month.id ==17 & (purchased$products!="") & purchased$ncodpers %in% intersect(extra.train.ids.test,ids.train))]

df$birthday.month   <- factor(month.abb[df$birthday.month],levels=month.abb)
test$birthday.month <- factor(month.abb[test$birthday.month],levels=month.abb)

df$month   <- factor(month.abb[df$month],levels=month.abb)
test$month <- factor(month.abb[test$month],levels=month.abb)

df <- select(df,-fecha_alta,-fecha_dato,-month.previous.id)

extra.train.val <- df %>% 
  filter(ncodpers %in% extra.train.ids.val & month.id %in% extra.train.months.val)

extra.train.test <- rbind(df %>% 
                            filter(ncodpers %in% extra.train.ids.test & month.id %in% extra.train.months.test),
                          df %>% 
                            filter(ncodpers %in% extra.train.ids.may & month.id ==17))


val.train <- df %>% 
  filter(ncodpers %in% ids.val.train & month.id %in% val.train.month)

val.test <- df %>% 
  filter(ncodpers %in% ids.val.test & month.id %in% val.test.month) 

df <- df %>% 
  filter(ncodpers %in% ids.train & month.id %in% train.month) 

test <- test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id)  

# val.test <- rbind(val.test,extra.train)
# df       <- rbind(df,extra.train)


# recent.birthday  <- (fread("cleaned_train.csv")) %>%
  # select(ncodpers,month.id,age,antiguedad)
# recent.birthday  <- (fread("test_ver2.csv")) %>%
  # select(ncodpers,month.id,age,antiguedad)
# tmp <-  merge(recent.birthday, 
                          # recent.birthday[,.(ncodpers,month.id=month.id+1,age,antiguedad)],
                          # by=c("ncodpers","month.id"))


# write.csv(df,"train_prepped.csv",row.names=FALSE)
# write.csv(test,"test_prepped.csv",row.names=FALSE)

save(df,test,val.train,val.test,extra.train.val,extra.train.test,file="data_prepped.RData")

