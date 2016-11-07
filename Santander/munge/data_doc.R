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
source("Santander/lib/helpers.R")
options(dplyr.width = Inf)

col_types <- cols(
  .default = col_integer(),
  fecha_dato = col_date(format = ""),
  ind_empleado = col_character(),
  pais_residencia = col_character(),
  sexo = col_character(),
  fecha_alta = col_date(format = ""),
  ult_fec_cli_1t = col_date(format = ""),
  indrel_1mes = col_character(),
  tiprel_1mes = col_character(),
  indresi = col_character(),
  indext = col_character(),
  conyuemp = col_character(),
  canal_entrada = col_character(),
  indfall = col_character(),
  nomprov = col_character(),
  renta = col_double(),
  segmento = col_character()
)
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
  map(quick_look) %>%
  bind_rows(.id = "Variable")

#+
#save(var_info, file = "Santander/cache/var_info.rdata")
load("Santander/cache/var_info.rdata")
kable(var_info, caption = "Quick Look at Variables", align = "c")



