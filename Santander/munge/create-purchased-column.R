# This script outputs a csv file containing a list of new products purchased 
# each month by each customer.
library(data.table)
# setwd('~/kaggle/competition-santander/')
df     <- fread("cleaned_train.csv")
labels <- names(df)[grepl("ind_+.*_+ult",names(df))]
cols   <- c("ncodpers","month.id","month.previous.id",labels)
df     <- df[,names(df) %in% cols,with=FALSE]

# connect each month to the previous one
df     <- merge(df,df,by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

# entries that don't have a corresponding row for the previous month will be NA and
# I will treat these as if that product was owned 
df[is.na(df)] <- 0

# for each product, the difference between the current month on the left and the
# previous month on the right indicates whether a product was added (+1), dropped (-1),
# or unchanged (0)
products <- rep("",nrow(df))
for (label in labels){
  colx  <- paste0(label,".x")
  coly  <- paste0(label,".y")
  diffs <- df[,.(get(colx)-get(coly))]
  products[diffs>0] <- paste0(products[diffs>0],label,sep=" ")
}

df <- df[,.(ncodpers,month.id,products)]
write.csv(df,"purchased-products.csv",row.names=FALSE)
