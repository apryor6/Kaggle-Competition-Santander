#' # Matt's Data Documentation
#' 
#' AJ has done an awesome job of jumping into the data and playing around with
#' distributions and missing values. I'm going to try to document the 
#' information we know already and create a report on the variables themselves.
#' This should make it easier for us to pick up the project if we get busy and
#' can't work on it every other day. 
#' 
#+ package_load, echo = F, eval = F
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
library(modeler) 
## My own internal package, install with
## devtools::install_github("mattmills49/modeler")

source("Santander/lib/helpers.R")
options(dplyr.width = Inf)
#+ readin_data, eval = F
train_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types)

account_data <- select(train_data, fecha_dato:segmento) %>% clean_names

quick_look <- function(x){
  var_info <- data_frame(Type = class(x)[1],
             Unique_Values = n_distinct(x),
             Num_missing = sum(is.na(x)))
  return(var_info)
}

var_info <- account_data %>%
  mutate(signup_time = as.numeric(difftime(fetch_date, signup_date, units = "days"))) %>%
  select(-customer_code, -dplyr::contains("date")) %>%
  peruse

#+ echo = F
#save(var_info, file = "Santander/cache/var_info.rdata")
load("Santander/cache/var_info.rdata")
kable(select(var_info, -data, -Type), align = "c")

#' ### Numeric Variables
#' 
#+ echo = F
var_info %>%
  filter(Class == "Numeric") %>%
  select(-Class) %>%
  unnest(data) %>% 
  kable(align = "c")

#' It looks like `new_customer`, `completed_month`, and `activity_index` are 
#' actually binary variables that we can turn into character values.
#' 
#' ### Character Variables
#' 
#+ echo = F
var_info %>%
  filter(Class != "Numeric") %>%
  select(-Class) %>%
  mutate(most_common = map_chr(data, ~ stringr::str_c(names(.x), " - ", .x, collapse = ", "))) %>% 
  select(-data) %>%
  kable(align = "c")

#' ### Variable Drift
#' 
#' The data is structured as monthly measurements for accounts for 17 months. 
#' I'm curious to find out how often the variables change month to month. 
#' 
#+

cal_data <- train_data %>%
  make_calibration %>%
  clean_names %>%
  select(fetch_date:segment)

cal_data %>% 
  sample_frac(.1) %>%
  group_by(customer_code) %>%
  summarise_all(n_distinct) %>% head
  
