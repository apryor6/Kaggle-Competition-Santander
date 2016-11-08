#' # Calibration / Training Split
#' 
#' We want to split our data so decisions we make about replacing missing 
#' values and variable transformations don't bias our results. I'll be 
#' splitting the accounts into a 40/60 split for calibration and training.
#' 
#+
library(readr)
library(dplyr)

accounts <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = cols_only(ncodpers = col_integer())) %>%
  distinct

set.seed(02101991)
cal_accounts <- sample_frac(accounts, .4)

saveRDS(cal_accounts, file = "Santander/cache/cal_accounts.rds")
