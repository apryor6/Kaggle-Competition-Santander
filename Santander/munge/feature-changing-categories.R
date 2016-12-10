library(data.table)
categories <- c("nomprov","age")


df <- rbind(fread("cleaned_train.csv"), fread("cleaned_test.csv"))
df <- df[month.id <= 6 | month.id >=13]
df <- as.data.frame(df)
num.residences <- df %>%
  group_by(ncodpers,section=month.id<7) %>%
  summarise(move.flag=length(unique(segmento)))

df.train <- df %>%
  filter(month.id == 6) %>%
  select(ncodpers,month.id) %>%
  merge(num.residences %>% filter(section==TRUE),by=c("ncodpers"))%>%
  select(-section)

df.train2 <- df %>%
  filter(month.id == 18) %>%
  select(ncodpers,month.id) %>%
  merge(num.residences %>% filter(section==FALSE),by=c("ncodpers")) %>%
  select(-section)

df.train <- rbind(df.train,df.train2)

df.train$move.flag <- ifelse(df.train$move.flag>1,1,0)

write.csv(df.train,"feature-recently-moved.csv",row.names = FALSE)



# df <- rbind(fread("cleaned_train.csv"), fread("cleaned_test.csv"))
# df <- df[month.id <= 6 | month.id >=13]
# df <- as.data.frame(df)
# num.residences <- df %>%
#   group_by(ncodpers,section=month.id<7) %>%
#   summarise(move.flag=length(unique(nomprov)))
# 
# df.train <- df %>%
#   filter(month.id == 6) %>%
#   select(ncodpers,month.id) %>%
#   merge(num.residences %>% filter(section==TRUE),by=c("ncodpers"))%>%
#   select(-section)
# 
# df.train2 <- df %>%
#   filter(month.id == 18) %>%
#   select(ncodpers,month.id) %>%
#   merge(num.residences %>% filter(section==FALSE),by=c("ncodpers")) %>%
#   select(-section)
# 
# df.train <- rbind(df.train,df.train2)
# 
# df.train$move.flag <- ifelse(df.train$move.flag>1,1,0)
# 
# write.csv(df.train,"feature-recently-moved.csv",row.names = FALSE)



