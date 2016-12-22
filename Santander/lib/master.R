# Master script for creating submission start to finish
setwd("~/kaggle/competition-santander/")

# Convert the .Rmd cleaning script to a regular R script
source('project/Santander/lib/rmd2rscript.R')
rmd2rscript("project/Santander/munge/santander-cleaning.Rmd")

# Clean the data
source("project/Santander/munge/santander-cleaning[rmd2r].R")

# Create file with products purchased each month
source("project/Santander/munge/create-purchased-column.R")

# Get purchase and transaction information
source("project/Santander/lib/purchases-by-month.R")

# Create purchase frequency feature
source("project/Santander/munge/feature-purchase-frequency.R")

# Engineer the rest of the features and do some minor cleaning before modeling
source("project/Santander/lib/engineer-features.R")

# Build models and make predictions
source("project/Santander/lib/model_xgboost_multiclass_ajp_best.R")
source("project/Santander/lib/model_xgboost_singleclass_ajp_best.R")
source("project/Santander/lib/model_xgboost_multiclass_ajp_1.R")
source("project/Santander/lib/model_xgboost_singleclass_ajp_1.R")
source("project/Santander/lib/model_xgboost_multiclass_ajp_2.R")
source("project/Santander/lib/model_xgboost_singleclass_ajp_2.R")
source("project/Santander/lib/model_xgboost_multiclass_ajp_3.R")
source("project/Santander/lib/model_xgboost_singleclass_ajp_3.R")


# Combine predictions from various models
source("project/Santander/lib/combine-predictions.R")

# Generate all recommendations and compute MAP@7 score on the validation data
source("project/Santander/lib/generate-recommendations.R")
