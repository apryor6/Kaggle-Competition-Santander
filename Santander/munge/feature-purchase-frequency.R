# This script outputs a csv file containing a list of new products purchased 
# each month by each customer.
library(data.table)
setwd('~/kaggle/competition-sandtander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[df$month.id<6,names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num.transactions <- rep(0,nrow(df))
# purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
# purchase.frequencies.small <- purchase.frequencies[purchase.frequencies$month.id>13,]
purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(ncodpers,month.id,change=get(colx)-get(coly))]
  num.transactions <- num.transactions + as.integer(diffs$change!=0)
  # diffs$change[diffs$month.id==1] <- 0
  diffs[diffs<0] <- 0
  setkey(diffs,ncodpers)
  d <- diffs[,.(frequency = cumsum(change)),by=ncodpers]
  # purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency / purchase.frequencies$month.id
  purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency

}
purchase.frequencies$num.transactions <- num.transactions
purchase.frequencies <- purchase.frequencies %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(num.transactions = cumsum(num.transactions))
write.csv(purchase.frequencies,"purchase.frequencies.csv",row.names=FALSE)





# This script outputs a csv file containing a list of new products purchased 
# each month by each customer.
library(data.table)
setwd('~/kaggle/competition-sandtander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[(df$month.id<18) & (df$month.id>12),names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num.transactions <- rep(0,nrow(df))

# purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
# purchase.frequencies.small <- purchase.frequencies[purchase.frequencies$month.id>13,]
purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(ncodpers,month.id,change=get(colx)-get(coly))]
  num.transactions <- num.transactions + as.integer(diffs$change!=0)
  
  # diffs$change[diffs$month.id==1] <- 0
  diffs[diffs<0] <- 0
  setkey(diffs,ncodpers)
  d <- diffs[,.(frequency = cumsum(change)),by=ncodpers]
  # purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency / purchase.frequencies$month.id
  purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency
  
  
}

purchase.frequencies$num.transactions <- num.transactions
purchase.frequencies <- purchase.frequencies %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(num.transactions = cumsum(num.transactions))
write.csv(purchase.frequencies,"purchase.frequencies.later.csv",row.names=FALSE)

# df <- df[,.(ncodpers,month.id,products)]
# write.csv(df,"purchased-products.csv",row.names=FALSE)
# print(dim(df[df$month.id==6 & df$products!="",]))
