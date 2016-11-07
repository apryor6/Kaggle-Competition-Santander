# Matt's Data Documentation

AJ has done an awesome job of jumping into the data and playing around with
distributions and missing values. I'm going to try to document the 
information we know already and create a report on the variables themselves.
This should make it easier for us to pick up the project if we get busy and
can't work on it every other day. 




```r
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
```

```r
#save(var_info, file = "Santander/cache/var_info.rdata")
load("Santander/cache/var_info.rdata")
kable(var_info, caption = "Quick Look at Variables", align = "c")
```



|        Variable        |   Type    | Unique_Values | Num_missing |
|:----------------------:|:---------:|:-------------:|:-----------:|
|       fetch_date       |   Date    |      17       |      0      |
|     customer_code      |  integer  |    956645     |      0      |
|     employee_index     | character |       6       |    27734    |
|   country_residence    | character |      119      |    27734    |
|          sex           | character |       3       |    27804    |
|          age           |  integer  |      121      |    27734    |
|      signup_date       |   Date    |     6757      |    27734    |
|      new_customer      |  integer  |       3       |    27734    |
|       seniority        |  integer  |      259      |    27734    |
|    completed_month     |  integer  |       3       |    27734    |
|       leave_date       |   Date    |      224      |  13622516   |
|     customer_type      |  numeric  |       5       |   150655    |
|   customer_relation    | character |       6       |   149781    |
| residence_same_as_bank | character |       3       |    27734    |
|       foreigner        | character |       3       |    27734    |
|    employee_spouse     | character |       3       |  13645501   |
|      join_channel      | character |      163      |   186126    |
|          dead          | character |       3       |    27734    |
|      addres_type       |  integer  |       2       |    27735    |
|     province_code      |  integer  |      53       |    93591    |
|     province_name      | character |      53       |    93591    |
|     activity_index     |  integer  |       3       |    27734    |
|      gross_income      |  numeric  |    520995     |   2794375   |
|        segment         | character |       4       |   189368    |

