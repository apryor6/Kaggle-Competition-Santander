Code Walkthrough
----------------

To start, we explore and clean the data. *Most of the following few sections were written by me at the very beginning of the competition when I was actually doing the exploration, and I have left the text mostly intact. Some approaches were changed later, such as the treatment of missing values, and I have added notes accordingly. Comments from future me are in italics*

~~~ r
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(fasttime)
~~~

#### ggplot2 Theme Trick

A cool trick to avoid repetitive code in `ggplot2` is to save/reuse your own theme. I'll build one here and use it throughout.

~~~ r
my_theme <- theme_bw() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))

my_theme_dark <- theme_dark() +
  theme(axis.title=element_text(size=24),
        plot.title=element_text(size=36),
        axis.text =element_text(size=16))
~~~

First Glance
------------

~~~ r
setwd("~/kaggle/competition-santander/")
set.seed(1)
df   <- (fread("train_ver2.csv"))
~~~

    ## 
    Read 0.0% of 13647309 rows
    Read 3.1% of 13647309 rows
    Read 7.8% of 13647309 rows
    Read 12.8% of 13647309 rows
    Read 18.1% of 13647309 rows
    Read 23.4% of 13647309 rows
    Read 27.8% of 13647309 rows
    Read 32.8% of 13647309 rows
    Read 37.9% of 13647309 rows
    Read 43.2% of 13647309 rows
    Read 48.5% of 13647309 rows
    Read 53.3% of 13647309 rows
    Read 58.2% of 13647309 rows
    Read 63.4% of 13647309 rows
    Read 68.7% of 13647309 rows
    Read 73.8% of 13647309 rows
    Read 79.0% of 13647309 rows
    Read 84.3% of 13647309 rows
    Read 89.5% of 13647309 rows
    Read 94.9% of 13647309 rows
    Read 99.6% of 13647309 rows
    Read 13647309 rows and 48 (of 48) columns from 2.135 GB file in 00:00:31

~~~ r
test <- (fread("test_ver2.csv"))
features <- names(df)[grepl("ind_+.*ult.*",names(df))]
~~~

I will create a label for each product and month that indicates whether a customer added, dropped or maintained that service in that billing cycle. I will do this by assigning a numeric id to each unique time stamp, and then matching each entry with the one from the previous month. The difference in the indicator value for each product then gives the desired value.
A cool trick to turn dates into unique id numbers is to use `as.numeric(factor(...))`. Make sure to order them chronologically first.

~~~ r
df                     <- df %>% arrange(fecha_dato) %>% as.data.table()
df$month.id            <- as.numeric(factor((df$fecha_dato)))
df$month.previous.id   <- df$month.id - 1
test$month.id          <- max(df$month.id) + 1
test$month.previous.id <- max(df$month.id)

# Test data will contain the status of products for the previous month, which is a feature. The training data currently contains the status of products as labels, and will later be joined to the previous month to get the previous month's ownership as a feature. I choose to do it in this order so that the train/test data can be cleaned together and then split. It's just for convenience.
test <- merge(test,df[,names(df) %in% c(features,"ncodpers","month.id"),with=FALSE],by.x=c("ncodpers","month.previous.id"),by.y=c("ncodpers","month.id"),all.x=TRUE)

df <- rbind(df,test)
~~~

We have a number of demographics for each individual as well as the products they currently own. To make a test set, I will separate the last month from this training data, and create a feature that indicates whether or not a product was newly purchased. First convert the dates. There's `fecha_dato`, the row-identifier date, and `fecha_alta`, the date that the customer joined.

~~~ r
df[,fecha_dato:=fastPOSIXct(fecha_dato)]
df[,fecha_alta:=fastPOSIXct(fecha_alta)]
unique(df$fecha_dato)
~~~

    ##  [1] "2015-01-27 16:00:00 PST" "2015-02-27 16:00:00 PST"
    ##  [3] "2015-03-27 17:00:00 PDT" "2015-04-27 17:00:00 PDT"
    ##  [5] "2015-05-27 17:00:00 PDT" "2015-06-27 17:00:00 PDT"
    ##  [7] "2015-07-27 17:00:00 PDT" "2015-08-27 17:00:00 PDT"
    ##  [9] "2015-09-27 17:00:00 PDT" "2015-10-27 17:00:00 PDT"
    ## [11] "2015-11-27 16:00:00 PST" "2015-12-27 16:00:00 PST"
    ## [13] "2016-01-27 16:00:00 PST" "2016-02-27 16:00:00 PST"
    ## [15] "2016-03-27 17:00:00 PDT" "2016-04-27 17:00:00 PDT"
    ## [17] "2016-05-27 17:00:00 PDT" "2016-06-27 17:00:00 PDT"

I printed the values just to double check the dates were in standard Year-Month-Day format. I expect that customers will be more likely to buy products at certain months of the year (Christmas bonuses?), so let's add a month column. I don't think the month that they joined matters, so just do it for one.

~~~ r
df$month <- month(df$fecha_dato)
~~~

Are there any columns missing values?

~~~ r
sapply(df,function(x)any(is.na(x)))
~~~

    ##            fecha_dato              ncodpers          ind_empleado 
    ##                 FALSE                 FALSE                 FALSE 
    ##       pais_residencia                  sexo                   age 
    ##                 FALSE                 FALSE                  TRUE 
    ##            fecha_alta             ind_nuevo            antiguedad 
    ##                  TRUE                  TRUE                  TRUE 
    ##                indrel        ult_fec_cli_1t           indrel_1mes 
    ##                  TRUE                 FALSE                  TRUE 
    ##           tiprel_1mes               indresi                indext 
    ##                 FALSE                 FALSE                 FALSE 
    ##              conyuemp         canal_entrada               indfall 
    ##                 FALSE                 FALSE                 FALSE 
    ##               tipodom              cod_prov               nomprov 
    ##                  TRUE                  TRUE                 FALSE 
    ## ind_actividad_cliente                 renta              segmento 
    ##                  TRUE                  TRUE                 FALSE 
    ##     ind_ahor_fin_ult1     ind_aval_fin_ult1      ind_cco_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_cder_fin_ult1      ind_cno_fin_ult1     ind_ctju_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_ctma_fin_ult1     ind_ctop_fin_ult1     ind_ctpp_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_deco_fin_ult1     ind_deme_fin_ult1     ind_dela_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_ecue_fin_ult1     ind_fond_fin_ult1      ind_hip_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_plan_fin_ult1     ind_pres_fin_ult1     ind_reca_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_tjcr_fin_ult1     ind_valo_fin_ult1      ind_viv_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##       ind_nomina_ult1     ind_nom_pens_ult1       ind_recibo_ult1 
    ##                  TRUE                  TRUE                 FALSE 
    ##              month.id     month.previous.id                 month 
    ##                 FALSE                 FALSE                 FALSE

Definitely. Onto data cleaning.

Data Cleaning
-------------

Going down the list, start with `age`

~~~ r
ggplot(data=df,aes(x=age)) +
  geom_bar(alpha=0.75,fill="tomato",color="black") +
  xlim(c(18,100)) +
  ggtitle("Age Distribution") +
  my_theme
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-8-1.png)

In addition to NA, there are people with very small and very high ages. It's also interesting that the distribution is bimodal. There are a large number of university aged students, and then another peak around middle-age. Let's separate the distribution and move the outliers to the mean of the closest one. I also add a feature indicating in which month the person's birthday is -- maybe you are more likely to add products then. *I later changed missing values to -1 as a flag and got **slightly** better results. It seems some predictive power is contained in the lack of information itself. I also later discovered that the first 6 months of this dataset appear to be backfilled and are stagnant. For example, `antiguedad` (the number of months an account has existed) does not increment at all for the first 6 months. Here I use the person's birthday to backcorrect the ages. This might seem like a small thing to do, but there is a harsh cutoff at age 20 for ownership of junior accounts, so this little detail matters.*

~~~ r
# df$age[(df$age < 18)] <- median(df$age[(df$age >= 18) & (df$age <=30)],na.rm=TRUE)
# df$age[(df$age > 100)] <- median(df$age[(df$age >= 30) & (df$age <=100)],na.rm=TRUE)
# df$age[is.na(df$age)] <- median(df$age,na.rm=TRUE)

age.change  <- df[month.id>6,.(age,month,month.id,age.diff=c(0,diff(age))),by="ncodpers"]
age.change  <- age.change[age.diff==1]
age.change  <- age.change[!duplicated(age.change$ncodpers)]
setkey(df,ncodpers)
df <- merge(df,age.change[,.(ncodpers,birthday.month=month)],by=c("ncodpers"),all.x=TRUE,sort=FALSE)
df$birthday.month[is.na(df$birthday.month)] <- 7 # July is the only month we don't get to check for increment so if there is no update then use it
df$age[df$birthday.month <= 7 & df$month.id<df$birthday.month] <- df$age[df$birthday.month <= 7 & df$month.id<df$birthday.month]  - 1 # correct ages in the first 6 months

df$age[is.na(df$age)] <- -1

df$age <- round(df$age)
~~~

*I flip back and forth between `dplyr` and `data.table`, so sometimes you'll see me casting things back and forth like this.*

~~~ r
df <- as.data.frame(df)
~~~

Next `ind_nuevo`, which indicates whether a customer is new or not. How many missing values are there?

~~~ r
sum(is.na(df$ind_nuevo))
~~~

    ## [1] 27734

Let's see if we can fill in missing values by looking how many months of history these customers have.

~~~ r
months.active <- df[is.na(df$ind_nuevo),] %>%
  group_by(ncodpers) %>%
  summarise(months.active=n())  %>%
  select(months.active)
max(months.active)
~~~

    ## [1] 6

Looks like these are all new customers, so replace accordingly.

~~~ r
df$ind_nuevo[is.na(df$ind_nuevo)] <- 1 
~~~

Now, `antiguedad`

~~~ r
sum(is.na(df$antiguedad))
~~~

    ## [1] 27734

That number again. Probably the same people that we just determined were new customers. Double check.

~~~ r
summary(df[is.na(df$antiguedad),]%>%select(ind_nuevo))
~~~

    ##    ind_nuevo
    ##  Min.   :1  
    ##  1st Qu.:1  
    ##  Median :1  
    ##  Mean   :1  
    ##  3rd Qu.:1  
    ##  Max.   :1

\*This feature is the number of months since the account joined and suffers from the stagnation issue I mentioned previously in the first 6 months, and here I correct it. Many customers have a valid value for `fecha_alta`, the month that they joined, and this can be used to recompute `antiguedad`. For entries without `fecha_alta`, I assume the value of `antiguedad` at month 6 is correct and correct the rest accordingly.

~~~ r
new.antiguedad <- df %>% 
  dplyr::select(ncodpers,month.id,antiguedad) %>%
  dplyr::group_by(ncodpers) %>%
  dplyr::mutate(antiguedad=min(antiguedad,na.rm=T) + month.id - 6) %>% #month 6 is the first valid entry, so reassign based upon that reference
  ungroup() %>%
  dplyr::arrange(ncodpers) %>%
  dplyr::select(antiguedad)
df <- df %>%
  arrange(ncodpers) # arrange so that the two data frames are aligned
df$antiguedad <- new.antiguedad$antiguedad

df$antiguedad[df$antiguedad<0] <- -1

elapsed.months <- function(end_date, start_date) {
  12 * (year(end_date) - year(start_date)) + (month(end_date) - month(start_date))
}
recalculated.antiguedad <- elapsed.months(df$fecha_dato,df$fecha_alta)
df$antiguedad[!is.na(df$fecha_alta)] <- recalculated.antiguedad[!is.na(df$fecha_alta)]
df$ind_nuevo <- ifelse(df$antiguedad<=6,1,0) # reassign new customer index
~~~

Some entries don't have the date they joined the company. Just give them something in the middle of the pack

~~~ r
df$fecha_alta[is.na(df$fecha_alta)] <- median(df$fecha_alta,na.rm=TRUE)
~~~

Next is `indrel`, which indicates:

> 1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)

This sounds like a promising feature. I'm not sure if primary status is something the customer chooses or the company assigns, but either way it seems intuitive that customers who are dropping down are likely to have different purchasing behaviors than others.

~~~ r
table(df$indrel)
~~~

    ## 
    ##        1       99 
    ## 14522714    26476

Fill in missing with the more common status.

~~~ r
df$indrel[is.na(df$indrel)] <- 1
~~~

> tipodom - Addres type. 1, primary address cod\_prov - Province code (customer's address)

`tipodom` doesn't seem to be useful, and the province code is not needed becaue the name of the province exists in `nomprov`.

~~~ r
df <- df %>% select(-tipodom,-cod_prov)
~~~

Quick check back to see how we are doing on missing values

~~~ r
sapply(df,function(x)any(is.na(x)))
~~~

    ##              ncodpers            fecha_dato          ind_empleado 
    ##                 FALSE                 FALSE                 FALSE 
    ##       pais_residencia                  sexo                   age 
    ##                 FALSE                 FALSE                 FALSE 
    ##            fecha_alta             ind_nuevo            antiguedad 
    ##                 FALSE                  TRUE                  TRUE 
    ##                indrel        ult_fec_cli_1t           indrel_1mes 
    ##                 FALSE                 FALSE                  TRUE 
    ##           tiprel_1mes               indresi                indext 
    ##                 FALSE                 FALSE                 FALSE 
    ##              conyuemp         canal_entrada               indfall 
    ##                 FALSE                 FALSE                 FALSE 
    ##               nomprov ind_actividad_cliente                 renta 
    ##                 FALSE                  TRUE                  TRUE 
    ##              segmento     ind_ahor_fin_ult1     ind_aval_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##      ind_cco_fin_ult1     ind_cder_fin_ult1      ind_cno_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_ctju_fin_ult1     ind_ctma_fin_ult1     ind_ctop_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_ctpp_fin_ult1     ind_deco_fin_ult1     ind_deme_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_dela_fin_ult1     ind_ecue_fin_ult1     ind_fond_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##      ind_hip_fin_ult1     ind_plan_fin_ult1     ind_pres_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##     ind_reca_fin_ult1     ind_tjcr_fin_ult1     ind_valo_fin_ult1 
    ##                 FALSE                 FALSE                 FALSE 
    ##      ind_viv_fin_ult1       ind_nomina_ult1     ind_nom_pens_ult1 
    ##                 FALSE                  TRUE                  TRUE 
    ##       ind_recibo_ult1              month.id     month.previous.id 
    ##                 FALSE                 FALSE                 FALSE 
    ##                 month        birthday.month 
    ##                 FALSE                 FALSE

Getting closer.

~~~ r
sum(is.na(df$ind_actividad_cliente))
~~~

    ## [1] 27734

By now you've probably noticed that this number keeps popping up. A handful of the entries are just bad, and should probably just be excluded from the model. But for now I will just clean/keep them.
*I ultimately ended up keeping these entries and just kept the missing values separated*

Just a couple more features.

~~~ r
df$ind_actividad_cliente[is.na(df$ind_actividad_cliente)] <- median(df$ind_actividad_cliente,na.rm=TRUE)
~~~

~~~ r
unique(df$nomprov)
~~~

    ##  [1] "MADRID"                 "BIZKAIA"               
    ##  [3] "PALMAS, LAS"            "BURGOS"                
    ##  [5] "CADIZ"                  "ALICANTE"              
    ##  [7] "ZAMORA"                 "BARCELONA"             
    ##  [9] "GIPUZKOA"               "GUADALAJARA"           
    ## [11] ""                       "SEVILLA"               
    ## [13] "GRANADA"                "CORUÑA, A"             
    ## [15] "VALENCIA"               "BADAJOZ"               
    ## [17] "CANTABRIA"              "MALAGA"                
    ## [19] "ALMERIA"                "PONTEVEDRA"            
    ## [21] "ALAVA"                  "GIRONA"                
    ## [23] "AVILA"                  "MURCIA"                
    ## [25] "SALAMANCA"              "SANTA CRUZ DE TENERIFE"
    ## [27] "SEGOVIA"                "JAEN"                  
    ## [29] "TOLEDO"                 "CACERES"               
    ## [31] "NAVARRA"                "ASTURIAS"              
    ## [33] "HUELVA"                 "LERIDA"                
    ## [35] "LUGO"                   "ZARAGOZA"              
    ## [37] "BALEARS, ILLES"         "PALENCIA"              
    ## [39] "TARRAGONA"              "VALLADOLID"            
    ## [41] "CIUDAD REAL"            "CASTELLON"             
    ## [43] "OURENSE"                "RIOJA, LA"             
    ## [45] "CORDOBA"                "ALBACETE"              
    ## [47] "CUENCA"                 "HUESCA"                
    ## [49] "LEON"                   "MELILLA"               
    ## [51] "SORIA"                  "TERUEL"                
    ## [53] "CEUTA"

There's some rows missing a city that I'll relabel

~~~ r
df$nomprov[df$nomprov==""] <- "UNKNOWN"
~~~

Now for gross income, aka `renta`

~~~ r
sum(is.na(df$renta))
~~~

    ## [1] 3022340

Here is a feature that is missing a lot of values. Rather than just filling them in with a median, it's probably more accurate to break it down region by region. To that end, let's take a look at the median income by region, and in the spirit of the competition let's color it like the Spanish flag.

~~~ r
df %>%
  filter(!is.na(renta)) %>%
  group_by(nomprov) %>%
  summarise(med.income = median(renta)) %>%
  arrange(med.income) %>%
  mutate(city=factor(nomprov,levels=nomprov)) %>%
  ggplot(aes(x=city,y=med.income)) +
  geom_point(color="#c60b1e") +
  guides(color=FALSE) +
  xlab("City") +
  ylab("Median Income") +
  my_theme +
  theme(axis.text.x=element_blank(), axis.ticks = element_blank()) +
  geom_text(aes(x=city,y=med.income,label=city),angle=90,hjust=-.25) +
  theme(plot.background=element_rect(fill="#c60b1e"),
        panel.background=element_rect(fill="#ffc400"),
        panel.grid =element_blank(),
        axis.title =element_text(color="#ffc400"),
        axis.text  =element_text(color="#ffc400"),
        plot.title =element_text(color="#ffc400")) +
  ylim(c(60000,180000)) +


  ggtitle("Income Distribution by City")
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-27-1.png)

There's a lot of variation, so I think assigning missing incomes by providence is a good idea. This code gets kind of confusing in a nested SQL statement kind of way, but the idea is to first group the data by city, and reduce to get the median. This intermediate data frame is joined by the original city names to expand the aggregated median incomes, ordered so that there is a 1-to-1 mapping between the rows, and finally the missing values are replaced.
*Same story, I ended up not doing this and just treating missing values separately*

~~~ r
# new.incomes <-df %>%
#   select(nomprov) %>%
#   merge(df %>%
#   group_by(nomprov) %>%
#   dplyr::summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
#   select(nomprov,med.income) %>%
#   arrange(nomprov)
# df <- arrange(df,nomprov)
# df$renta[is.na(df$renta)] <- new.incomes$med.income[is.na(df$renta)]
# rm(new.incomes)
# 
# df$renta[is.na(df$renta)] <- median(df$renta,na.rm=TRUE)
df$renta[is.na(df$renta)] <- -1
~~~

The last line is to account for any values that are still missing. For example, it seems every entry from Alava has NA for `renta`.

The only remaining missing value are for features

~~~ r
sum(is.na(df$ind_nomina_ult1))
~~~

    ## [1] 16063

I could try to fill in missing values for products by looking at previous months, but since it's such a small number of values for now I'll take the cheap way out.

~~~ r
df[is.na(df)] <- 0
~~~

Now we have taken care of all the missing values. There's also a bunch of character columns that can contain empty strings, so we need to go through them. For the most part, entries with empty strings will be converted to an unknown category.

~~~ r
str(df)
~~~

    ## 'data.frame':    14576924 obs. of  50 variables:
    ##  $ ncodpers             : int  15889 15889 15889 15889 15889 15889 15889 15889 15889 15889 ...
    ##  $ fecha_dato           : POSIXct, format: "2015-01-27 16:00:00" "2015-02-27 16:00:00" ...
    ##  $ ind_empleado         : chr  "F" "F" "F" "F" ...
    ##  $ pais_residencia      : chr  "ES" "ES" "ES" "ES" ...
    ##  $ sexo                 : chr  "V" "V" "V" "V" ...
    ##  $ age                  : num  55 55 55 55 55 55 56 56 56 56 ...
    ##  $ fecha_alta           : POSIXct, format: "1995-01-15 16:00:00" "1995-01-15 16:00:00" ...
    ##  $ ind_nuevo            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ antiguedad           : num  240 241 242 243 244 245 246 247 248 249 ...
    ##  $ indrel               : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ult_fec_cli_1t       : chr  "" "" "" "" ...
    ##  $ indrel_1mes          : chr  "1" "1" "1" "1" ...
    ##  $ tiprel_1mes          : chr  "A" "A" "A" "A" ...
    ##  $ indresi              : chr  "S" "S" "S" "S" ...
    ##  $ indext               : chr  "N" "N" "N" "N" ...
    ##  $ conyuemp             : chr  "N" "N" "N" "N" ...
    ##  $ canal_entrada        : chr  "KAT" "KAT" "KAT" "KAT" ...
    ##  $ indfall              : chr  "N" "N" "N" "N" ...
    ##  $ nomprov              : chr  "MADRID" "MADRID" "MADRID" "MADRID" ...
    ##  $ ind_actividad_cliente: num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ renta                : num  326125 326125 326125 326125 326125 ...
    ##  $ segmento             : chr  "01 - TOP" "01 - TOP" "01 - TOP" "01 - TOP" ...
    ##  $ ind_ahor_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_aval_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_cco_fin_ult1     : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ind_cder_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_cno_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_ctju_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_ctma_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_ctop_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_ctpp_fin_ult1    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ind_deco_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_deme_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_dela_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_ecue_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_fond_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_hip_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_plan_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_pres_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_reca_fin_ult1    : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_tjcr_fin_ult1    : int  1 0 0 0 1 1 1 0 0 0 ...
    ##  $ ind_valo_fin_ult1    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ind_viv_fin_ult1     : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_nomina_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_nom_pens_ult1    : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ ind_recibo_ult1      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ month.id             : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ month.previous.id    : num  0 1 2 3 4 5 6 7 8 9 ...
    ##  $ month                : num  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ birthday.month       : num  7 7 7 7 7 7 7 7 7 7 ...

~~~ r
char.cols <- names(df)[sapply(df,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(df[[name]]))
  }
~~~

    ## [1] "Unique values for ind_empleado:"
    ## [1] "F" "A" "N" "B" ""  "S"
    ## [1] "Unique values for pais_residencia:"
    ##   [1] "ES" ""   "PT" "NL" "AD" "IN" "VE" "US" "FR" "GB" "IT" "DE" "MX" "CL"
    ##  [15] "CO" "CH" "CR" "PE" "JP" "AT" "AR" "AE" "BE" "MA" "CI" "QA" "SE" "BR"
    ##  [29] "FI" "RS" "KE" "JM" "RU" "AU" "CU" "EC" "KR" "DO" "LU" "GH" "CZ" "PA"
    ##  [43] "IE" "BO" "CM" "CA" "GR" "ZA" "RO" "KH" "IL" "NG" "CN" "DK" "NZ" "MM"
    ##  [57] "SG" "UY" "NI" "EG" "GI" "PH" "KW" "VN" "TH" "NO" "GQ" "BY" "AO" "UA"
    ##  [71] "TR" "PL" "OM" "GA" "GE" "BG" "HR" "PR" "HK" "HN" "BA" "MD" "HU" "SK"
    ##  [85] "TN" "TG" "SA" "MR" "DZ" "LB" "MT" "SV" "PK" "PY" "LY" "MK" "EE" "SN"
    ##  [99] "MZ" "GT" "GN" "TW" "IS" "LT" "CD" "KZ" "BZ" "CF" "GM" "ET" "SL" "GW"
    ## [113] "LV" "CG" "ML" "BM" "ZW" "AL" "DJ"
    ## [1] "Unique values for sexo:"
    ## [1] "V" "H" "" 
    ## [1] "Unique values for ult_fec_cli_1t:"
    ##   [1] ""           "2015-08-05" "2016-03-17" "2015-12-04" "2016-03-29"
    ##   [6] "2015-07-15" "2016-04-12" "2015-12-17" "2015-10-27" "2016-04-19"
    ##  [11] "2016-05-24" "2015-07-03" "2016-02-23" "2016-05-30" "2015-10-05"
    ##  [16] "2015-07-27" "2016-02-24" "2015-12-03" "2015-11-16" "2015-12-01"
    ##  [21] "2016-05-17" "2015-09-29" "2016-01-26" "2016-01-05" "2016-05-20"
    ##  [26] "2015-11-12" "2016-03-02" "2016-04-01" "2016-03-16" "2015-07-22"
    ##  [31] "2016-06-23" "2015-12-09" "2015-10-13" "2016-03-18" "2015-11-11"
    ##  [36] "2015-11-10" "2015-08-20" "2015-08-03" "2015-08-19" "2015-10-06"
    ##  [41] "2016-05-02" "2015-10-02" "2016-02-11" "2015-07-20" "2016-05-27"
    ##  [46] "2015-07-21" "2016-06-01" "2016-03-04" "2016-04-22" "2015-12-11"
    ##  [51] "2015-09-08" "2015-12-15" "2016-04-15" "2015-07-28" "2016-02-08"
    ##  [56] "2016-05-26" "2015-12-02" "2015-09-10" "2015-11-27" "2016-06-24"
    ##  [61] "2015-10-21" "2015-08-24" "2015-12-28" "2015-11-02" "2016-01-19"
    ##  [66] "2016-01-18" "2016-03-15" "2015-07-17" "2016-01-22" "2015-09-01"
    ##  [71] "2015-08-18" "2015-10-22" "2015-07-07" "2016-03-28" "2015-09-21"
    ##  [76] "2016-01-13" "2016-02-16" "2015-09-23" "2015-11-23" "2016-06-28"
    ##  [81] "2015-12-10" "2015-12-24" "2015-11-24" "2016-05-09" "2016-02-01"
    ##  [86] "2016-04-06" "2015-10-28" "2016-05-18" "2016-01-28" "2015-08-13"
    ##  [91] "2016-04-05" "2016-02-15" "2015-09-15" "2015-10-01" "2015-10-20"
    ##  [96] "2016-02-09" "2016-03-23" "2015-07-06" "2016-03-14" "2016-04-07"
    ## [101] "2016-01-11" "2016-06-03" "2016-02-12" "2015-07-14" "2015-09-11"
    ## [106] "2015-09-03" "2016-01-21" "2016-02-04" "2016-06-06" "2015-12-16"
    ## [111] "2015-11-19" "2015-08-06" "2015-12-21" "2015-08-27" "2015-11-18"
    ## [116] "2016-04-26" "2016-05-12" "2015-07-01" "2016-01-15" "2015-08-14"
    ## [121] "2016-02-25" "2015-09-14" "2016-03-07" "2016-01-12" "2016-04-08"
    ## [126] "2015-12-30" "2015-11-17" "2016-06-07" "2016-01-08" "2016-06-14"
    ## [131] "2015-08-25" "2015-09-28" "2015-09-24" "2016-06-22" "2016-01-07"
    ## [136] "2015-11-13" "2016-06-27" "2016-03-11" "2015-09-25" "2015-08-28"
    ## [141] "2015-11-25" "2016-01-25" "2016-05-06" "2015-08-04" "2015-08-10"
    ## [146] "2015-09-22" "2015-10-26" "2016-04-21" "2016-01-04" "2016-03-08"
    ## [151] "2015-12-14" "2015-09-18" "2016-05-25" "2016-03-10" "2016-03-01"
    ## [156] "2016-02-10" "2016-06-17" "2016-05-05" "2015-08-17" "2015-10-15"
    ## [161] "2016-06-10" "2015-11-04" "2015-10-07" "2016-06-21" "2015-09-02"
    ## [166] "2016-04-20" "2015-08-21" "2016-05-23" "2015-11-03" "2016-05-04"
    ## [171] "2015-07-30" "2015-07-09" "2015-10-19" "2016-05-10" "2015-07-29"
    ## [176] "2016-04-14" "2015-08-26" "2015-09-04" "2016-02-02" "2016-06-08"
    ## [181] "2015-12-22" "2016-02-26" "2015-10-09" "2015-09-17" "2016-01-27"
    ## [186] "2016-02-22" "2016-06-09" "2016-03-22" "2015-10-08" "2015-12-18"
    ## [191] "2016-02-03" "2016-03-30" "2016-04-28" "2016-04-13" "2016-06-15"
    ## [196] "2016-06-13" "2016-01-20" "2015-11-05" "2015-12-29" "2016-04-18"
    ## [201] "2016-06-16" "2015-07-02" "2015-11-20" "2016-04-27" "2015-12-07"
    ## [206] "2015-11-06" "2016-05-03" "2015-07-13" "2016-02-17" "2015-10-14"
    ## [211] "2015-08-11" "2015-10-29" "2016-06-20" "2016-04-04" "2016-02-18"
    ## [216] "2016-06-29" "2015-07-10" "2015-07-16" "2015-07-23" "2015-10-23"
    ## [221] "2016-05-19" "2016-05-11" "2015-12-23" "2015-11-26" "2015-11-09"
    ## [226] "2016-04-11" "2016-03-09" "2015-08-12" "2015-07-08" "2015-09-07"
    ## [231] "2016-03-21" "2015-09-09" "2016-01-14" "2015-09-16" "2016-03-24"
    ## [236] "2015-10-16" "2016-02-19" "2016-04-25" "2016-06-02" "2016-02-05"
    ## [241] "2016-05-16" "2016-03-03" "2016-05-13" "2015-08-07" "2015-07-24"
    ## [1] "Unique values for indrel_1mes:"
    ##  [1] "1"   "2"   "1.0" "2.0" "4.0" ""    "3.0" "3"   "P"   "4"   "0"  
    ## [1] "Unique values for tiprel_1mes:"
    ## [1] "A" "I" "P" ""  "R" "N"
    ## [1] "Unique values for indresi:"
    ## [1] "S" ""  "N"
    ## [1] "Unique values for indext:"
    ## [1] "N" "S" "" 
    ## [1] "Unique values for conyuemp:"
    ## [1] "N" "S" "" 
    ## [1] "Unique values for canal_entrada:"
    ##   [1] "KAT" "013" "KFA" ""    "KFC" "KAS" "007" "KHK" "KCH" "KHN" "KHC"
    ##  [12] "KFD" "RED" "KAW" "KHM" "KCC" "KAY" "KBW" "KAG" "KBZ" "KBO" "KEL"
    ##  [23] "KCI" "KCG" "KES" "KBL" "KBQ" "KFP" "KEW" "KAD" "KDP" "KAE" "KAF"
    ##  [34] "KAA" "KDO" "KEY" "KHL" "KEZ" "KAH" "KAR" "KHO" "KHE" "KBF" "KBH"
    ##  [45] "KAO" "KFV" "KDR" "KBS" "KCM" "KAC" "KCB" "KAL" "KGX" "KFT" "KAQ"
    ##  [56] "KFG" "KAB" "KAZ" "KGV" "KBJ" "KCD" "KDU" "KAJ" "KEJ" "KBG" "KAI"
    ##  [67] "KBM" "KDQ" "KAP" "KDT" "KBY" "KAM" "KFK" "KFN" "KEN" "KDZ" "KCA"
    ##  [78] "KHQ" "KFM" "004" "KFH" "KFJ" "KBR" "KFE" "KDX" "KDM" "KDS" "KBB"
    ##  [89] "KFU" "KAV" "KDF" "KCL" "KEG" "KDV" "KEQ" "KAN" "KFL" "KDY" "KEB"
    ## [100] "KGY" "KBE" "KDC" "KBU" "KAK" "KBD" "KEK" "KCF" "KED" "KAU" "KEF"
    ## [111] "KFR" "KCU" "KCN" "KGW" "KBV" "KDD" "KCV" "KBP" "KCT" "KBN" "KCX"
    ## [122] "KDA" "KCO" "KCK" "KBX" "KDB" "KCP" "KDE" "KCE" "KEA" "KDG" "KDH"
    ## [133] "KEU" "KFF" "KEV" "KEC" "KCS" "KEH" "KEI" "KCR" "KDW" "KEO" "KHS"
    ## [144] "KFS" "025" "KFI" "KHD" "KCQ" "KDN" "KHR" "KEE" "KEM" "KFB" "KGU"
    ## [155] "KCJ" "K00" "KDL" "KDI" "KGC" "KHA" "KHF" "KGN" "KHP"
    ## [1] "Unique values for indfall:"
    ## [1] "N" "S" "" 
    ## [1] "Unique values for nomprov:"
    ##  [1] "MADRID"                 "BIZKAIA"               
    ##  [3] "PALMAS, LAS"            "BURGOS"                
    ##  [5] "CADIZ"                  "ALICANTE"              
    ##  [7] "ZAMORA"                 "BARCELONA"             
    ##  [9] "GIPUZKOA"               "GUADALAJARA"           
    ## [11] "UNKNOWN"                "SEVILLA"               
    ## [13] "GRANADA"                "CORUÑA, A"             
    ## [15] "VALENCIA"               "BADAJOZ"               
    ## [17] "CANTABRIA"              "MALAGA"                
    ## [19] "ALMERIA"                "PONTEVEDRA"            
    ## [21] "ALAVA"                  "GIRONA"                
    ## [23] "AVILA"                  "MURCIA"                
    ## [25] "SALAMANCA"              "SANTA CRUZ DE TENERIFE"
    ## [27] "SEGOVIA"                "JAEN"                  
    ## [29] "TOLEDO"                 "CACERES"               
    ## [31] "NAVARRA"                "ASTURIAS"              
    ## [33] "HUELVA"                 "LERIDA"                
    ## [35] "LUGO"                   "ZARAGOZA"              
    ## [37] "BALEARS, ILLES"         "PALENCIA"              
    ## [39] "TARRAGONA"              "VALLADOLID"            
    ## [41] "CIUDAD REAL"            "CASTELLON"             
    ## [43] "OURENSE"                "RIOJA, LA"             
    ## [45] "CORDOBA"                "ALBACETE"              
    ## [47] "CUENCA"                 "HUESCA"                
    ## [49] "LEON"                   "MELILLA"               
    ## [51] "SORIA"                  "TERUEL"                
    ## [53] "CEUTA"                 
    ## [1] "Unique values for segmento:"
    ## [1] "01 - TOP"           "02 - PARTICULARES"  ""                  
    ## [4] "03 - UNIVERSITARIO"

Okay, based on that and the definitions of each variable, I will fill the empty strings either with the most common value or create an unknown category based on what I think makes more sense.

~~~ r
df$indfall[df$indfall==""]                 <- "N"
df$tiprel_1mes[df$tiprel_1mes==""]         <- "A"
df$indrel_1mes[df$indrel_1mes==""]         <- "1"
df$indrel_1mes[df$indrel_1mes=="P"]        <- "5"
df$indrel_1mes <- as.factor(as.integer(df$indrel_1mes))

df$pais_residencia[df$pais_residencia==""] <- "UNKNOWN"
df$sexo[df$sexo==""]                       <- "UNKNOWN"
df$ult_fec_cli_1t[df$ult_fec_cli_1t==""]   <- "UNKNOWN"
df$ind_empleado[df$ind_empleado==""]       <- "UNKNOWN"
df$indext[df$indext==""]                   <- "UNKNOWN"
df$indresi[df$indresi==""]                 <- "UNKNOWN"
df$conyuemp[df$conyuemp==""]               <- "UNKNOWN"
df$segmento[df$segmento==""]               <- "UNKNOWN"
~~~

Convert all the features to numeric dummy indicators (you'll see why in a second), and we're done cleaning

~~~ r
features <- grepl("ind_+.*ult.*",names(df))
df[,features] <- lapply(df[,features],function(x)as.integer(round(x)))
~~~

### Lag Features

-   Very important to this competition were so-called lag features, meaning that for each entry it was beneficial to consider not only the value of a feature for the current month, but also the value for previous months. Soon after discovering that lagged product ownership was a useful feature (i.e. whether or not a product was owned 1,2,3,4,etc months ago), I figured it was possible to use other lagged features. Here is a function that makes it easy to create such features. The idea is to join the data by account id, `ncodpers`, and to match the month with the lag month. For example, to add a 2-month lag feature to an observation in month 5, we want to extract the value of `feature.name` at month 3.\*

~~~ r
# create-lag-feature.R

create.lag.feature <- function(dt, # should be a data.table!
                               feature.name, # name of the feature to lag
                               months.to.lag=1,# vector of integers indicating how many months to lag
                               by=c("ncodpers","month.id"), # keys to join data.tables by
                               na.fill = NA)  
  {
  # get the feature and change the name to avoid .x and .y being appending to names
  dt.sub <- dt[,mget(c(by,feature.name))]
  names(dt.sub)[names(dt.sub) == feature.name] <- "original.feature"
  original.month.id <- dt.sub$month.id
  added.names <- c()
  for (month.ago in months.to.lag){
    print(paste("Collecting information on",feature.name,month.ago,"month(s) ago"))
    colname <- paste("lagged.",feature.name,".",month.ago,"months.ago",sep="")
    added.names <- c(colname,added.names)
    # This is a self join except the month is shifted
    dt.sub <- merge(dt.sub,
                    dt.sub[,.(ncodpers,
                                        month.id=month.ago+original.month.id,
                                        lagged.feature=original.feature)],
                    by=by,
                    all.x=TRUE,
                    sort=FALSE)
    names(dt.sub)[names(dt.sub)=="lagged.feature"] <- colname
    # dt.sub[[colname]][is.na(dt.sub[[colname]])] <- dt.sub[["original.feature"]][is.na(dt.sub[[colname]])]
  }
  df <- merge(dt,
              dt.sub[,c(by,added.names),with=FALSE],
              by=by,
              all.x=TRUE,
              sort=FALSE)
  df[is.na(df)] <- na.fill
  return(df)
}
~~~

*Now I use that function to create lagged features of `ind_actividad_cliente`, the customer activity index. For a few percent of customers I noticed that `ind_actividad_cliente` was almost perfectly correlated with one of a few products (particularly `ind_tjcr_fin_ult1` (credit card), `ind_cco_fin_ult1` (current accounts), and `ind_recibo_ult1` (debit account)). I think this is actually a leak in the dataset, as it appears such a customer was marked as active because they used a product. Therefore, I thought this was going to be an extremely powerful feature, but it turned out to not provide much, if any, benefit. My conclusion was that although this was a useful predictor for that few percent of customers, the problem is being unable to identify which accounts followed this trend. To me it seems `ind_actividad_cliente` is recorded with high inconsistency. Some customers own many products and are marked inactive, while others are marked active but own nothing. Maybe one of the teams who outperformed us figured out how to utilize this information.*

~~~ r
source('~/kaggle/competition-santander/project/Santander/lib/create-lag-feature.R')
df <- as.data.table(df)
df <- create.lag.feature(df,'ind_actividad_cliente',1:11,na.fill=0)
~~~

    ## [1] "Collecting information on ind_actividad_cliente 1 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 2 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 3 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 4 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 5 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 6 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 7 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 8 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 9 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 10 month(s) ago"
    ## [1] "Collecting information on ind_actividad_cliente 11 month(s) ago"

*Junior accounts, `ind_ctju_fin_ult1`, are for those 19 and younger. I found that the month that a customer turned 20 there was a discontinuation of `ind_ctju_fin_ult1` followed by a high likelihood of adding e-accounts, `ind_ecue_fin_ult1`. I add a binary feature to capture this.*

~~~ r
df[,last.age:=lag(age),by="ncodpers"]
df$turned.adult <- ifelse(df$age==20 & df$last.age==19,1,0)
df <- as.data.frame(df)
~~~

Now the data is cleaned, separate it back into train/test. I'm writing a csv file because I will do some other analysis that uses these files, but if you are ever just saving variables to use them again in R you should write binary files with `save` and `load` -- they are *way* faster.

~~~ r
features <- names(df)[grepl("ind_+.*ult.*",names(df))]

test <- df %>%
  filter(month.id==max(df$month.id))
df <- df %>%
  filter(month.id<max(df$month.id))
write.csv(df,"cleaned_train.csv",row.names=FALSE)
write.csv(test,"cleaned_test.csv",row.names=FALSE)
~~~

Data Visualization
------------------

*These are some of the plots I made at the beginning of the competition to get a sense of what the data was like and to get preliminary ideas for features. I will say, however, that most of the useful insights ultimately came from a combination of XGBoost feature importance outputs and from going through the raw data for many accounts by hand.* To study trends in customers adding or removing services, I will create a label for each product and month that indicates whether a customer added, dropped or maintained that service in that billing cycle. I will do this by assigning a numeric id to each unique time stamp, and then matching each entry with the one from the previous month. The difference in the indicator value for each product then gives the desired value.
A cool trick to turn dates into unique id numbers is to use `as.numeric(factor(...))`. Make sure to order them chronologically first.

~~~ r
features          <- grepl("ind_+.*ult.*",names(df))
df[,features]     <- lapply(df[,features],function(x)as.integer(round(x)))
df$total.services <- rowSums(df[,features],na.rm=TRUE)
df                <- df %>% arrange(fecha_dato)
df$month.id       <- as.numeric(factor((df$fecha_dato)))
df$month.next.id  <- df$month.id + 1
~~~

Now I'll build a function that will convert differences month to month into a meaningful label. Each month, a customer can either maintain their current status with a particular product, add it, or drop it.

~~~ r
status.change <- function(x){
  if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
    label = ifelse(x==1,"Added","Maintained")
  } else {
    diffs <- diff(x) # difference month-by-month
    diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
    label <- rep("Maintained", length(x))
    label <- ifelse(diffs==1,"Added",
                    ifelse(diffs==-1,"Dropped",
                           "Maintained"))
  }
  label
}
~~~

Now we can actually apply this function to each feature using `lapply` and `ave`

~~~ r
df[,features] <- lapply(df[,features], function(x) return(ave(x,df$ncodpers, FUN=status.change)))
~~~

I'm only interested in seeing what influences people adding or removing services, so I'll trim away any instances of "Maintained". Since big melting/casting operations can be slow, I'll take the time to check for rows that should be completely removed, then melt the remainder and remove the others.

~~~ r
interesting <- rowSums(df[,features]!="Maintained")
df          <- df[interesting>0,]
df          <- df %>%
                gather(key=feature,
                value=status,
                ind_ahor_fin_ult1:ind_recibo_ult1)
df          <- filter(df,status!="Maintained")
head(df)
~~~

    ##   ncodpers month.id          fecha_dato ind_empleado pais_residencia sexo
    ## 1    53273        2 2015-02-27 16:00:00            N              ES    V
    ## 2   352884        4 2015-04-27 17:00:00            N              ES    H
    ## 3   507549        4 2015-04-27 17:00:00            N              ES    V
    ## 4   186769        5 2015-05-27 17:00:00            N              ES    V
    ## 5   186770        5 2015-05-27 17:00:00            N              ES    H
    ## 6   110663        6 2015-06-27 17:00:00            N              ES    V
    ##   age          fecha_alta ind_nuevo antiguedad indrel ult_fec_cli_1t
    ## 1  42 2000-04-05 17:00:00         0        178      1        UNKNOWN
    ## 2  41 2002-04-21 17:00:00         0        156      1        UNKNOWN
    ## 3  49 2004-12-19 16:00:00         0        124      1        UNKNOWN
    ## 4  49 2000-08-02 17:00:00         0        177      1        UNKNOWN
    ## 5  46 2000-08-02 17:00:00         0        177      1        UNKNOWN
    ## 6  38 2002-06-16 17:00:00         0        156      1        UNKNOWN
    ##   indrel_1mes tiprel_1mes indresi indext conyuemp canal_entrada indfall
    ## 1           1           A       S      N  UNKNOWN           KAT       N
    ## 2           1           A       S      N  UNKNOWN           KAT       N
    ## 3           1           I       S      N  UNKNOWN           KAT       N
    ## 4           1           A       S      N  UNKNOWN           KAT       N
    ## 5           1           A       S      N  UNKNOWN           KAT       N
    ## 6           1           A       S      N  UNKNOWN           KAT       N
    ##                  nomprov ind_actividad_cliente    renta          segmento
    ## 1              BARCELONA                     1 121920.3          01 - TOP
    ## 2             VALLADOLID                     1  60303.3 02 - PARTICULARES
    ## 3 SANTA CRUZ DE TENERIFE                     0  99163.8 02 - PARTICULARES
    ## 4                 MADRID                     1     -1.0          01 - TOP
    ## 5                 MADRID                     1     -1.0 02 - PARTICULARES
    ## 6                 MADRID                     1 395567.8 02 - PARTICULARES
    ##   month.previous.id month birthday.month
    ## 1                 1     2              6
    ## 2                 3     4              2
    ## 3                 3     4              1
    ## 4                 4     5              1
    ## 5                 4     5              9
    ## 6                 5     6              9
    ##   lagged.ind_actividad_cliente.11months.ago
    ## 1                                         0
    ## 2                                         0
    ## 3                                         0
    ## 4                                         0
    ## 5                                         0
    ## 6                                         0
    ##   lagged.ind_actividad_cliente.10months.ago
    ## 1                                         0
    ## 2                                         0
    ## 3                                         0
    ## 4                                         0
    ## 5                                         0
    ## 6                                         0
    ##   lagged.ind_actividad_cliente.9months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        0
    ##   lagged.ind_actividad_cliente.8months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        0
    ##   lagged.ind_actividad_cliente.7months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        0
    ##   lagged.ind_actividad_cliente.6months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        0
    ##   lagged.ind_actividad_cliente.5months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        0
    ## 5                                        0
    ## 6                                        1
    ##   lagged.ind_actividad_cliente.4months.ago
    ## 1                                        0
    ## 2                                        0
    ## 3                                        0
    ## 4                                        1
    ## 5                                        1
    ## 6                                        1
    ##   lagged.ind_actividad_cliente.3months.ago
    ## 1                                        0
    ## 2                                        1
    ## 3                                        0
    ## 4                                        1
    ## 5                                        1
    ## 6                                        1
    ##   lagged.ind_actividad_cliente.2months.ago
    ## 1                                        0
    ## 2                                        1
    ## 3                                        0
    ## 4                                        1
    ## 5                                        1
    ## 6                                        1
    ##   lagged.ind_actividad_cliente.1months.ago last.age turned.adult
    ## 1                                        1       42            0
    ## 2                                        1       41            0
    ## 3                                        0       49            0
    ## 4                                        1       49            0
    ## 5                                        1       46            0
    ## 6                                        1       38            0
    ##   total.services month.next.id           feature  status
    ## 1              6             3 ind_ahor_fin_ult1   Added
    ## 2              4             5 ind_ahor_fin_ult1 Dropped
    ## 3              0             5 ind_ahor_fin_ult1 Dropped
    ## 4              8             6 ind_ahor_fin_ult1 Dropped
    ## 5              6             6 ind_ahor_fin_ult1 Dropped
    ## 6              2             7 ind_ahor_fin_ult1 Dropped

Does the ratio of dropping/adding services change over the year?

~~~ r
totals.by.feature <- df %>%
  group_by(month,feature) %>%
  summarise(counts=n())

df %>% 
  group_by(month,feature,status) %>%
  summarise(counts=n())%>%
  ungroup() %>%
  inner_join(totals.by.feature,by=c("month","feature")) %>%

  mutate(counts=counts.x/counts.y) %>%
  ggplot(aes(y=counts,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)]))) +
  geom_bar(aes(fill=status), stat="identity") +
  facet_wrap(facets=~feature,ncol = 6) +
  coord_flip() +
  my_theme_dark + 
  ylab("Count") +
  xlab("") + 
  ylim(limits=c(0,1)) +
  ggtitle("Relative Service \nChanges by Month") +
  theme(axis.text   = element_text(size=10),
        legend.text = element_text(size=14),
        legend.title= element_blank()      ,
        strip.text  = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-43-1.png)

Let's see how product changes vary over the calendar year. Some months occur more than others, so we need to account for that.

~~~ r
month.counts              <- table(unique(df$month.id)%%12)
cur.names                 <- names(month.counts)
cur.names[cur.names=="0"] <- "12"
names(month.counts) <- cur.names
month.counts              <- data.frame(month.counts) %>%
  rename(month=Var1,month.count=Freq) %>% mutate(month=as.numeric(month))

df %>% 
  group_by(month,feature,status) %>%
  summarise(counts=n())%>%
  ungroup() %>%
  inner_join(month.counts,by="month") %>%

  mutate(counts=counts/month.count) %>%
  ggplot(aes(y=counts,x=factor(month.abb[month],levels=month.abb[seq(12,1,-1)]))) +
  geom_bar(aes(fill=status), stat="identity") +
  facet_wrap(facets=~feature,ncol = 6) +
  coord_flip() +
  my_theme_dark + 
  ylab("Count") +
  xlab("") + 
  ggtitle("Average Service \nChanges by Month") +
  theme(axis.text    = element_text(size=10),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-44-1.png)

~~~ r
df %>%
  filter(sexo!="UNKNOWN") %>%
  ggplot(aes(x=sexo)) +
  geom_bar(aes(fill=status)) +
  facet_wrap(facets=~feature,ncol = 6) +
  my_theme_dark + 
  ylab("Count") +
  xlab("") +
  ggtitle("Service Changes by Gender") +
  theme(axis.text    = element_text(size=10),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-45-1.png)

~~~ r
tot.H  <- sum(df$sexo=="H")
tot.V  <- sum(df$sexo=="V")
tmp.df <- df %>%
  group_by(sexo,status) %>%
  summarise(counts=n())
tmp.df$counts[tmp.df$sexo=="H"] = tmp.df$counts[tmp.df$sexo=="H"] / tot.H
tmp.df$counts[tmp.df$sexo=="V"] = tmp.df$counts[tmp.df$sexo=="V"] / tot.V
tmp.df %>%
  filter(sexo!="UNKNOWN") %>%
  ggplot(aes(x=factor(feature),y=counts)) +
  geom_bar(aes(fill=status,sexo),stat='identity') +
  coord_flip() +
  my_theme_dark + 
  ylab("Ratio") +
  xlab("") +
  ggtitle("Normalized Service \n Changes by Gender") +
  theme(axis.text    = element_text(size=20),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-46-1.png)

~~~ r
rm(tmp.df)
~~~

~~~ r
tot.new     <- sum(df$ind_nuevo==1)
tot.not.new <- sum(df$ind_nuevo!=1)
tmp.df      <- df %>%
  group_by(ind_nuevo,status) %>%
  summarise(counts=n())
tmp.df$counts[tmp.df$ind_nuevo==1] = tmp.df$counts[tmp.df$ind_nuevo==1] / tot.new
tmp.df$counts[tmp.df$ind_nuevo!=1] = tmp.df$counts[tmp.df$ind_nuevo!=1] / tot.not.new
tmp.df %>%
  ggplot(aes(x=factor(feature),y=counts)) +
  geom_bar(aes(fill=status,factor(ind_nuevo)),stat='identity') +
  coord_flip() +
  my_theme_dark + 
  ylab("Count") +
  xlab("") +
  ggtitle("Normalized Service \n Changes by New Status") +
  theme(axis.text    = element_text(size=10),
        legend.text  = element_text(size=14),
        legend.title = element_blank()      ,
        strip.text   = element_text(face="bold")) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-47-1.png)

~~~ r
rm(tmp.df)
~~~

~~~ r
df %>%
  group_by(nomprov,status) %>%
  summarise(y=mean(total.services)) %>%
  ggplot(aes(x=factor(nomprov,levels=sort(unique(nomprov),decreasing=TRUE)),y=y)) +
  geom_bar(stat="identity",aes(fill=status)) +
  geom_text(aes(label=nomprov),
            y=0.2,
            hjust=0,
            angle=0,
            size=3,
            color="#222222") +
  coord_flip() +
  my_theme_dark +
  xlab("City") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes\n by City") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-48-1.png)

~~~ r
df %>%
  group_by(antiguedad,status) %>%
  summarise(counts=n()) %>%
  ggplot(aes(x=factor(antiguedad),y=log(counts))) +
  geom_point(alpha=0.6,aes(color=status)) +
  my_theme_dark +
  xlab("Seniority (Months)") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes \n by Seniority") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_color_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-49-1.png)

~~~ r
df %>%
  ggplot(aes(x=age,y=log(renta))) +
  geom_point(alpha=0.5,aes(color=status)) +
  my_theme_dark +
  xlab("Age") +
  ylab("Income (log scale)") + 
  ggtitle("Income vs. Age") +
  theme(
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_color_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-50-1.png)

~~~ r
df %>%
  group_by(ncodpers) %>%
  slice(c(1,n())) %>%
  select(age,seniority=antiguedad,status) %>%
  ggplot(aes(x=age,y=seniority)) +
  geom_point(alpha=0.4,aes(color=status)) +
  ggtitle("Seniority vs. Age") + 
  my_theme_dark + 
  scale_color_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-51-1.png)

~~~ r
df %>%
  group_by(nomprov,status) %>%
  summarise(y=mean(total.services)) %>%
  ggplot(aes(x=factor(nomprov,levels=sort(unique(nomprov),decreasing=TRUE)),y=y)) +
  geom_bar(stat="identity",aes(fill=status)) +
  geom_text(aes(label=nomprov),
            y=0.2,
            hjust=0,
            angle=0,
            size=3,
            color="#222222") +
  coord_flip() +
  my_theme_dark +
  xlab("City") +
  ylab("Total # Changes") + 
  ggtitle("Service Changes\n by City") +
  theme(axis.text    = element_blank(),
        legend.text  = element_text(size=14),
        legend.title = element_text(size=18)) +
  scale_fill_manual(values=c("cyan","magenta"))
~~~

![](../images/Kaggle-Competition-Santander/Solution_p1_files/figure-markdown_github/unnamed-chunk-52-1.png)
