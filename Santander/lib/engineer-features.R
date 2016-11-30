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
df   <- (fread("cleaned_train.csv"))
test <- as.data.frame(fread("cleaned_test.csv"))
drop.products <- c("ind_ahor_fin_ult1","ind_aval_fin_ult1")

df   <- df[,!names(df) %in% drop.products,with=FALSE]
test <- test[,!names(test) %in% drop.products]
products <- names(df)[grepl("ind_+.*_+ult",names(df))]

products.owned <- df %>%
  filter(month.id <= 6 | month.id >=13) %>%
  select(ncodpers,month.id,one_of(products)) %>%
  as.data.table()

test <- as.data.table(test)
test <- test[,!names(test) %in% products,with=FALSE] #lazy, but I'm removing product ownership because it is about to be readded month by month
# df <- merge(df,df %>%
# dplyr::select(ind_cco_fin_ult1:ind_recibo_ult1, month.id, ncodpers),by.x=c("ncodpers","month.previous.id"), by.y=c("ncodpers","month.id"),all.x=TRUE)
original.month.id <- products.owned$month.id
df <- df[fecha_dato=="2015-06-28",]
for (month.ago in 1:5){
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
rm(list=c("products.owned","original.month.id"))

df <- as.data.frame(df)
test <- as.data.frame(test)
df[is.na(df)] <- 0
test[is.na(test)] <- 0
df$total_products <- rowSums(df[,names(df) %in% names(df)[grepl("1month\\_ago",names(df))]],na.rm=TRUE)
test$total_products <- rowSums(test[,names(test) %in% names(test)[grepl("1month\\_ago",names(test))]],na.rm=TRUE)

# df <- df %>%
# filter(fecha_dato%in%c("2015-06-28"))
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

# new.names <- names(test)
# new.names[new.names %in% products] <- paste(new.names[new.names %in% products],"_target",sep="")
# names(test) <- new.names

new.names <- names(df)
new.names[new.names %in% products] <- paste(new.names[new.names %in% products],"_target",sep="")
names(df) <- new.names

labels <- names(df)[grepl(".*_target",names(df))]
purchase.w <- names(df)[grepl(".*.count",names(df))]
# products <- names(df)[grepl("ind_+.*_+ult",names(df)) & !grepl(".*_target|.count|month\\_ago",names(df))]
ownership.names <- names(df)[grepl("month\\_ago",names(df))]
numeric.cols <- c("age","renta","antiguedad",purchase.w,"total_products","num.transactions")

categorical.cols <- c("sexo","ind_nuevo","ind_empleado","segmento",
                      "conyuemp","nomprov","indfall","indext","indresi",ownership.names)

print(labels)

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

df <- df %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id)

test <- test %>% 
  dplyr::select(-fecha_alta,-fecha_dato,-month.previous.id) %>%
  as.data.frame()

write.csv(df,"train_prepped.csv",row.names=FALSE)
write.csv(test,"test_prepped.csv",row.names=FALSE)