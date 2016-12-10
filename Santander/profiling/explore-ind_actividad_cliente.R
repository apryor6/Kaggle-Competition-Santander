df <- fread("train_ver2.csv")
df <- arrange(df,fecha_dato)
df$month.id <- as.integer(as.factor(df$fecha_dato))
# df <- df[month.id<=18 & month.id >=13]
names1 <- unique(df$ncodpers[df$ind_actividad_cliente==0])
names2 <- unique(df$ncodpers[df$ind_actividad_cliente==1])
df <- df[ncodpers %in% intersect(names1,names2)]
df <- df %>% arrange(ncodpers)
names <- unique(df$ncodpers)
df[ncodpers==names[15]]

tmp <- df[,.(a = sum(abs(diff(ind_actividad_cliente)))),by="ncodpers"]
d2 <- df[ncodpers %in% tmp$ncodpers[tmp$a>1]]
names <- unique(d2$ncodpers)
d2[ncodpers==names[18]]
