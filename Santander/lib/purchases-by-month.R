library(data.table)
library(dplyr)
df <- fread("purchased-products.csv")
full <- fread("cleaned_train.csv")
labels <- grepl("\\_ult1",names(full))
full$total.products <- rowSums(full[,labels,with=FALSE])
df[["counts"]] <- sapply(strsplit(df$products," "),length)
df <- merge(df,full[,.(month.id,ncodpers,total.products)],by=c("month.id","ncodpers"),all.x=TRUE,sort=FALSE)
# df <- df %>%
  # mutate(counts=(function(x)length(strsplit(x," ")))(products))
purchase.count <- rbind(data.table(ncodpers=df$ncodpers,month.id=df$month.id),
                        data.table(ncodpers=unique(df$ncodpers),month.id=18))
original.month.id <- df$month.id
for (month.ago in 1:10){
  print(paste("Collecting number of purchases",month.ago,"months ago"))
  colname <- paste("num.purchases.",month.ago,".months.ago",sep="")
  df[,month.id:=original.month.id + month.ago]
  tmp <- merge(purchase.count,df[,.(ncodpers,month.id,counts)],by=c("ncodpers","month.id"),sort=FALSE,all.x=TRUE)
  purchase.count[[colname]] <- tmp$counts
  
}

for (month.ago in 1:11){
  print(paste("Counting total products",month.ago,"months ago"))

  colname <- paste("total.products.",month.ago,".months.ago",sep="")
  df[,month.id:=original.month.id + month.ago]
  tmp <- merge(purchase.count,df[,.(ncodpers,month.id,total.products)],by=c("ncodpers","month.id"),sort=FALSE,all.x=TRUE)
  purchase.count[[colname]] <- tmp$total.products
}

purchase.count[is.na(purchase.count)] <- 0
write.csv(purchase.count,"purchase-count.csv",row.names=FALSE)
