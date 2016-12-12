create.lag.feature <- function(dt, 
                               feature.name, 
                               months.to.lag=1,
                               by=c("ncodpers","month.id"),
                               na.fill = NA) 
  {
  dt.sub <- dt[,mget(c(by,feature.name))]
  names(dt.sub)[names(dt.sub) == feature.name] <- "original.feature"
  original.month.id <- dt.sub$month.id
  added.names <- c()
  for (month.ago in months.to.lag){
    print(paste("Collecting information on",feature.name,month.ago,"month(s) ago"))
    colname <- paste("lagged.",feature.name,".",month.ago,"months.ago",sep="")
    added.names <- c(colname,added.names)
    dt.sub <- merge(dt.sub,
                    dt.sub[,.(ncodpers,
                                        month.id=month.ago+original.month.id,
                                        lagged.feature=original.feature)],
                    by=by,
                    all.x=TRUE,
                    sort=FALSE)
    names(dt.sub)[names(dt.sub)=="lagged.feature"] <- colname
    # dt.sub[[colname]][is.na(dt.sub[[colname]])] <- dt.sub[["original.feature"]][is.na(dt.sub[[colname]])]
  }
  df <- merge(df,
              dt.sub[,c(by,added.names),with=FALSE],
              by=by,
              all.x=TRUE,
              sort=FALSE)
  df[is.na(df)] <- na.fill
  return(df)
}