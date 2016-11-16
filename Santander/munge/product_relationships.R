#' # Purchase Relationships
#' 
#' I'm curious how the products relate to eachother. I want to see the 
#' correlation between which products each customer owns in the same month.
#+ echo = F
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))
source("Santander/lib/helpers.R")
options(dplyr.width = Inf)

#+ same_month, fig.height = 10, fig.width = 8, dpi = 200
raw_data <- read_csv("~/Documents/Data/Kaggle_Comps/Santander/train_ver2.csv", col_types = col_types, progress = F)

cal_data <- raw_data %>%
  make_calibration %>%
  clean_names

product_cors <- cal_data %>%
  select(savings:direct_debit) %>%
  cor(use = "complete.obs")

product_cors %>% 
  data.frame %>% 
  mutate(Variable_1 = row.names(product_cors)) %>%
  gather(Variable_2, Correlation, -Variable_1) %>%
  filter(Variable_1 != Variable_2) %>%
  ggplot(aes(x = Variable_1, y = Variable_2, fill = Correlation)) +
  geom_tile(color = "black") +
  viridis::scale_fill_viridis() +
  theme_mells +
  xlab("") +
  ylab("") +
  ggtitle("Correlation Between Products (Same Month)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        legend.position = "top")

#' But what I'm actually interested in is how having one product in a month
#' relates to having a product in the *next* month. 
#+ next_month
cal_data %>%
  arrange(customer_code) %>%
  head(100) %>%
  group_by(customer_code) %>%
  mutate(test = lag(credit_card)) %>% 
  select(customer_code, fetch_date, credit_card, test) %>% View


