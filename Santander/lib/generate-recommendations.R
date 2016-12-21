# This script combines information about product ownership in the previous month
# and predicted probability of owning a product in the current month to produce
# recommendations of currently unowned products
source('project/Santander/lib/get_recommendations.R')
source('project/Santander/lib/MAP.R')

val  <- fread("xgboost_preds_val_future.csv")
purchased <- as.data.frame(fread("purchased-products.csv"))

# val  <- fread("combined_preds_val.csv") # filename for ensemble version
# val$products <- val$products.x

load("project/Santander/lib/products.Rdata")

# get the recommendations
val.recs  <- get.recommendations(val,products)
val$added_products <- val.recs$added_products

val <- val %>%
  merge(purchased,by=c("ncodpers","month.id"))

# compute MAP@7 on the validation set
MAP <- mapk(k=7,strsplit(val$products, " "),strsplit(val$added_products," "))
print(paste("Validation MAP@7 = ",MAP))

# now predict on test 
test <- fread("xgboost_preds_test.csv")
# test <- fread("combined_preds_test.csv") # filename for ensemble version
test.recs <- get.recommendations(test,products)

# write out final submission
write.csv(test.recs,"recommendations_xgboost.csv",row.names = FALSE)
