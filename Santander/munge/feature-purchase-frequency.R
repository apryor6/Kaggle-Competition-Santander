# This script outputs a csv file containing the number of times each product was
# purchased in the last 5 months and the total number of transactions
library(data.table)
source('project/Santander/lib/create-lag-feature.R')
# setwd('~/kaggle/competition-santander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num.transactions <- rep(0,nrow(df))
purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(ncodpers,month.id,change=get(colx)-get(coly))]
  num.transactions <- num.transactions + as.integer(diffs$change!=0)
  diffs[diffs<0] <- 0
  setkey(diffs,ncodpers)
  d <- diffs[,.(frequency = cumsum(change)),by=ncodpers]
  purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency

}
purchase.frequencies$num.transactions <- num.transactions
purchase.frequencies <- purchase.frequencies %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(num.transactions = cumsum(num.transactions))
purchase.frequencies <- create.lag.feature(as.data.table(purchase.frequencies),
                                           "num.transactions",
                                           1:11,
                                           na.fill=0)
write.csv(purchase.frequencies,"purchase.frequencies.csv",row.names=FALSE)



# Now do the same for the testing data

df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df[is.na(df)] <- 0
products <- rep("",nrow(df))
num.transactions <- rep(0,nrow(df))

purchase.frequencies <- data.frame(ncodpers=df$ncodpers, month.id=(df$month.previous.id + 2))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(ncodpers,month.id,change=get(colx)-get(coly))]
  num.transactions <- num.transactions + as.integer(diffs$change!=0)
  diffs[diffs<0] <- 0
  setkey(diffs,ncodpers)
  d <- diffs[,.(frequency = cumsum(change)),by=ncodpers]
  purchase.frequencies[[paste(label,"_purchase.count",sep="")]] <- d$frequency
}

purchase.frequencies$num.transactions <- num.transactions
purchase.frequencies <- purchase.frequencies %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(num.transactions = cumsum(num.transactions))
purchase.frequencies <- create.lag.feature(as.data.table(purchase.frequencies),
                                           "num.transactions",
                                           1:11,
                                           na.fill=0)
write.csv(purchase.frequencies,"purchase.frequencies.later.csv",row.names=FALSE)
