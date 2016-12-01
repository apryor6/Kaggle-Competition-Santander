#' # h2o script
#' 
#+ 
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(h2o))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
source("Santander/lib/helpers.R")
options(dplyr.width = Inf)

raw_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types, progress = F)

train_data <- raw_data %>%
  make_train %>%
  clean_names

h2o.init(nthreads = 3, min_mem_size = "1G", max_mem_size = "4G")

#' Some things to note about h2o models:
#' * All character variables need to be factors
#' * You need the positions/names of the x values in the model OR you can leave
#'   the argument blank and it will use all the variables that aren't y.
#+
x_vars <- names(train_data) %>%
  magrittr::extract(3:24) %>%
  discard(str_detect, "date")
  
y_var <- "savings"

h2o_ready_data <- train_data %>%
  select_(.dots = as.list(c(x_vars, y_var))) %>%
  dmap_if(is.character, as.factor)
  
train_h2o_data <- as.h2o(h2o_ready_data)

savings_logistic <- h2o.glm(x = x_vars,
                         y = "savings",
                         training_frame = train_h2o_data,
                         family = "binomial",
                         lambda = 0)
# ~8 million records in ~35 seconds

#' Can also do penalized elastic net models using `alpha` and cross validation using `lambda_search`, `nfolds`, and others. 
#' With all 8 million records this is still a long run, about 30 minutes. 
#' 
#+

savings_lasso <- h2o.glm(y = "savings",
                         training_frame = train_h2o_data,
                         family = "binomial", 
                         alpha = 1,
                         nfolds = 3,
                         lambda_search = T)

#' In addition to Lasso/Ridge can do GBM, Random Forest, and Deep Learning. 
#' For most of these algorithms they can balance classes while running the model.
#+

train_h2o_data$savings <- as.factor(train_h2o_data$savings)
savings_gbm <- h2o.gbm(y = "savings", 
                       training_frame = train_h2o_data,
                       distribution = "bernoulli",
                       ntrees = 1000, 
                       max_depth = 5,
                       learn_rate = .01,
                       min_rows = 100,
                       sample_rate = .667,
                       col_sample_rate_per_tree = .667)
# current speed is 14%/8 min


