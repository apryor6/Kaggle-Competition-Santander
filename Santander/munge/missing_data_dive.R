#' # Missing Data Exploration
#' 
#' While exploring variables I've noticed some interesting patterns with 
#' missing data and I want to see if there is anything we can learn from 
#' the variable to variable missingness
#+ package_load
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(ggplot2))
source("Santander/lib/helpers.R")
options(dplyr.width = Inf)

raw_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types, progress = F)

#+ missing_by_var, fig.height = 8, fig.width = 8, dpi = 100

account_data <- select(raw_data, fecha_dato:segmento) %>% 
  make_calibration %>%
  clean_names

account_data %>%
  map(~ data_frame(pct_missing = mean(is.na(.x)))) %>% 
  bind_rows(.id = "variable") %>%
  ggplot(aes(x = variable, y = pct_missing)) +
  geom_bar(stat = "identity") +
  xlab("") +
  ylab("Percentage Observations Missing") +
  ggtitle("Percentage of Missing Observations by Variable") +
  theme_mells +
  scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
  coord_flip() +
  theme(axis.text.y = element_text(face = "bold"))

#' Clearly there are 2 crazy bad variables, `leave_date` and `employee_spouse`,
#' with nearly 100% of their observations missing. `gross_income` is also
#' pretty bad with around 20% of it's observations missing. I'm going to remove
#' those and see what patterns exist with the rest of the data. 
#' 
#+ missing_around, fig.width = 8, fig.width = 8, dpi = 100

missing_data <- account_data %>%
  select(-leave_date, -employee_spouse, -gross_income, -customer_code) %>%
  is.na %>%
  magrittr::multiply_by(1)

missing_rows <- rowSums(missing_data) != 0

missing_data[missing_rows, ] %>%
  data.frame %>%
  mutate(obs = 1:n()) %>%
  gather(Variable, Missing, -obs) %>% 
  mutate(Missing = Missing == 1) %>%
  ggplot(aes(x = obs, y = Variable, fill = Missing)) +
  geom_raster() +
  ylab("") +
  xlab("Observation Number") +
  ggtitle("Patterns of Missing Data Between Variables") +
  theme_mells +
  viridis::scale_fill_viridis(discrete = T) +
  theme(axis.text.y = element_text(face = "bold", hjust = 1),
        legend.position = "top")

#' AJ brought up a good point that the observations in the data are sorted
#' sequentially by month. I just want to make sure we are right in this 
#' assumption.
#+ month_order

months <- unique(account_data$fetch_date)
  
month_positions <- vapply(months, function(x, all_months = account_data$fetch_date){
  c(min(which(all_months == x)), max(which(all_months == x)))
}, numeric(2)) %>% t

colnames(month_positions) <- c("First Observation", "Last Observation")
month_positions

#' So we have verified that the data is sorted by month. The issue is we also 
#' removed a lot of the observations that were in the data since we removed the
#' observations that weren't missing any values. Let's see if there are any 
#' patterns for missing values by month and account. 
#+ all_missing_values, fig.width = 8, fig.width = 8, dpi = 100

account_data %>%
  select(-leave_date, -employee_spouse, -gross_income, -customer_code) %>%
  mutate(fetch_date = as.character(fetch_date)) %>%
  group_by(fetch_date) %>%
  mutate_all(is.na) %>%
  summarize_all(mean) %>%
  gather(Variable, Missing, -fetch_date) %>% 
  ggplot(aes(x = fetch_date, y = Variable, fill = Missing)) +
  geom_raster() +
  xlab("") +
  ylab("") +
  ggtitle("Percentage of Missing Data by Month") +
  theme_mells +
  viridis::scale_fill_viridis(discrete = F, name = "") +
  theme(axis.text.y = element_text(face = "bold", hjust = 1),
        axis.text.x = element_text(face = "bold", hjust = 1, angle = 45),
        legend.position = "top")

#+ eval = F, echo = F

account_nums <- distinct(account_data, customer_code) %>%
  mutate(account_num = 1:n())

some_miss <- missing_data %>%
  data.frame %>%
  cbind(month = as.character(account_data$fetch_date), customer_code = account_data$customer_code) %>%
  filter(missing_rows) %>% 
  left_join(account_nums, by = c("customer_code")) %>% 
  select(-customer_code)

some_miss %>%
  gather(Variable, Missing, -month, -account_num) %>% 
  mutate(Missing = Missing == 1) %>% filter(month == "2015-01-28") %>%
  ggplot(aes(x = account_num, y = Variable, fill = Missing)) +
  geom_raster() +
  #facet_wrap(~month, nrow = 4, ncol = 5) +
  ylab("") +
  xlab("Observation Number") +
  ggtitle("Patterns of Missing Data Between Variables") +
  theme_mells +
  viridis::scale_fill_viridis(discrete = T) +
  theme(axis.text.y = element_text(face = "bold", hjust = 1),
        legend.position = "top")

