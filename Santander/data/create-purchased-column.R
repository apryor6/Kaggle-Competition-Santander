# This script outputs a csv file containing a list of new products purchased 
# each month by each customer.
library(data.table)
setwd('~/kaggle/competition-sandtander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+.*_",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",names(df)[grepl("ind_+.*_+.*_",names(df))])
df     <- df[,names(df) %in% cols,with=F]
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"))

products <- rep("",nrow(df))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(get(colx)-get(coly))]
  products[diffs>0] <- paste(products[diffs>0],label)
}

df <- df[,.(ncodpers,month.id,products)]
write.csv(df,"purchased-products.csv",row.names=FALSE)
