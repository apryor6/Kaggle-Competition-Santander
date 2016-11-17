make.hybrid <- function(x,df,product){
  hybrid <- paste(as.character(x),
                  as.character(df[[paste(product,"_pred",sep="")]]))
  return(hybrid)
}


paste.strings <- function(products){
  # print(products)
  paste(products,collapse=" ")
  # paste(products,sep=" ")
}

get.recommendations <- function(df,products,n.recs = 7){
for (product in products){
  df[[product]] <- make.hybrid(df[[product]],df,product)
}

df <- df[,!grepl("_pred",names(df)),with=FALSE]
df <- as.data.frame(melt(df,
                           id.vars      = c("ncodpers","month.id"),
                           measure.vars = products,
                           variable.name= "product",
                           value.name   = "score"))
df <- df %>%
  filter(grepl("0\\ ",score)) # only keep products that have potential to be added
df <- df %>%
  mutate(score=as.numeric(gsub("0\\ ","",score))) # re-extract the probability

df <- df %>%
  group_by(ncodpers,month.id) %>%
  arrange(desc(score)) %>%
  slice(1:n.recs) %>%
  dplyr::summarise(added_products=paste.strings(product)) %>%
  dplyr::select(ncodpers,added_products)

return(df)
}