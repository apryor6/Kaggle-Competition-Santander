months.since.owned<- function(dt,products,months.to.search,default.value = 999){
  
  for (product in products){
      print(paste("Finding months since owning",product))
      colname <- paste(product,".last.owned",sep="")
      dt[[colname]] <- default.value
    for (month.ago in seq(months.to.search,1,-1)){
      cur.colname <- paste(product,"_",month.ago,"month_ago",sep="")
      dt[[colname]][dt[[cur.colname]] == 1] <- month.ago
    }
  }
  return(dt)
  
}