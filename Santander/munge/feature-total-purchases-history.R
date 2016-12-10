df <- fread("purchased-products.csv")
df$number <- sapply(strsplit(df$products," "),length)
purch <- df %>% select(ncodpers,month.id)
purch <- rbind(purch,data.frame(ncodpers=unique(purch$ncodpers),month.id=max(purch$month.id)+1))
original.month.id <- df$month.id
for (month.ago in 1:4){
  print(paste("Counting purchases from", month.ago,"months ago"))
  colname <- paste("num.added.",month.ago,".months.ago",sep="") 
  tmp <- merge(purch,df[,.(ncodpers,month.id=original.month.id+month.ago,number)],
                 by=c("ncodpers","month.id"),sort=FALSE,all.x=TRUE)
  purch[[colname]] <- tmp$number
}
purch[is.na(purch)] <- 0
print("writing...")
write.csv(purch,"purchase-history.csv",row.names = FALSE)
