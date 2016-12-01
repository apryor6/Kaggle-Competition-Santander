# This script combines information about product ownership in the previous month
# and predicted probability of owning a product in the current month to produce
# recommendations of currently unowned products
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')
# 
# test <- fread("xgboost_preds_test.csv")
# val  <- fread("xgboost_preds_val.csv")
test <- fread("caret_preds_test.csv")
val  <- fread("caret_preds_val.csv")
load("project/Santander/lib/products.Rdata")
test.recs <- get.recommendations(test,products)
val.recs  <- get.recommendations(val,products)
val$added_products <- val.recs$added_products
purchased <- as.data.frame(fread("purchased-products.csv"))
val <- val %>%
  merge(purchased,by=c("ncodpers","month.id"))
MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
print(paste("Validation MAP@7 = ",MAP))
write.csv(test.recs,"recommendations_xgboost.csv",row.names = FALSE)
