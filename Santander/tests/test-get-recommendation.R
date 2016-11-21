source("get_recommendations.R")
library(data.table)
dt <- data.table(ncodpers=c(1,2,3),
                 a=c(1,0,0),
                 b=c(0,0,0),
                 a_pred=c(0.5,0.4,0.3),
                 b_pred=c(.4,.3,.5),
                 month.id=c(1,2,3))

recs <- get.recommendations(dt,products=c("a","b"))
print(recs)
