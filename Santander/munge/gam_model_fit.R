#' # Fitting a GAM with Smoothing Splines
#' 
#' I personally love smoothing splines because they allow you to estimate 
#' non-linear relationships without picking one yourself and also take a lot
#' of evidence to make something really non-linear. Outliers become a bigger 
#' issue than linear models but we will deal with them when we get there. Also
#' the GAMs won't take missing values so we need to replace those. 
#' 
#' ### High Cardinality Transformations
#' 
#' Some of the variables have a lot of levels which cause the model to explode
#' when you use dummy variables for each level. To fix this you can use some of
#' the data to estimate the average y_value of each level, then just use those
#' estimates as your input variable into your model. You can read more about 
#' the approach here: http://dl.acm.org/citation.cfm?id=507538. 
#' 
#' First we will need to find the variables we want to do this to and replace
#' any missing values with "NA" so that we can use it as level. 
#' 
#+ data_prep
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(ggplot2))
source("Santander/lib/helpers.R")
options(dplyr.width = Inf)

raw_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types, progress = F)
       
cal_data <- raw_data %>%
  make_calibration %>%
  clean_names

train_data <- raw_data %>%
  make_train %>%
  clean_names

cal_data$province_code <- as.character(cal_data$province_code)
big_cols <- c("country_residence", "join_channel", "province_name", "province_code")

cleaned_factors <- cal_data %>%
  select(savings:direct_debit, country_residence, join_channel, province_name, province_code) %>%
  dmap_at(big_cols, ~ ifelse(is.na(.x), "NA", .x)) %>%
  dmap_at(big_cols, ~ ifelse(.x == "", "UNKNOWN", .x))

smoother_binomial <- function(x, y){
  group_names <- sort(unique(x)) # Find levels of factors
  group_counts <- vapply(group_names, function(gr) sum(x == gr), numeric(1))
  group_yes <- vapply(group_names, function(gr) sum(y[x == gr]), numeric(1))
  group_probs <- group_yes / group_counts
  
  u <- mean(group_probs)
  v <- var(group_probs)
  a <- ((1 - u) / v - 1 / u) * u * u
  b <- a * (1 / u - 1)
  
  posterior <- (group_yes + a) / (a + b + group_counts)
  return(dplyr::data_frame(level = group_names, estimate = posterior))
}

estimates <- cleaned_factors %>%
  select(country_residence:province_code) %>%
  map(smoother_binomial, cleaned_factors$direct_debit)

for(e in names(estimates)){
  new_vars <- estimates[[e]]
  names(new_vars)[1] <- e
  change_name <- list("estimate")
  names(change_name) <- paste0(e, "_estimate")
  cleaned_factors <- cleaned_factors %>%
    left_join(new_vars, by = names(estimates[i])) %>%
    rename_(.dots = change_name)
}