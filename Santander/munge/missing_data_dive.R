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
#+ readin_data, eval = F
raw_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types)

#+ missing_by_var, fig.height = 8, fig.width = 6, dpi = 100

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
#+ missing_around, fig.width = 8, fig.width = 6, dpi = 100

missing_data <- account_data %>%
  select(-leave_date, -employee_spouse, -gross_income) %>%
  is.na %>%
  magrittr::multiply_by(1)

missing_rows <- rowSums(missing_data) != 0

missing_data[missing_rows, ] %>%
  data.frame %>%
  mutate(obs = 1:n()) %>%
  gather(Variable, Missing, -obs) %>% 
  mutate(Missing = Missing == 1) %>%
  ggplot(aes(x = Variable, y = obs, fill = Missing)) +
  geom_raster() +
  xlab("") +
  ylab("Observation Number") +
  ggtitle("Patterns of Missing Data Between Variables")
  theme_mells +
  viridis::scale_fill_viridis(discrete = T) +
  theme(axis.text.y = element_text(face = "bold", hjust = 1),
        legend.position = "top") +
    coord_flip()
  
