df.cor <- function(df1,df2){
  v1 <- data.frame()
  v2 <- data.frame()
  for (col.number in 1:ncol(df1)){
    v1 <- rbind(v1,data.frame(df1[[col.number]]))
    v2 <- rbind(v2,data.frame(df2[[col.number]]))
  }
  return(cor(v1,v2)[1])
}