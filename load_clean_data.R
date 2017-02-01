setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_Santander_Product_R")


library(data.table)
library(lubridate)

#Load the data
train <- fread("./data/train_ver2.csv",nrows=-1, na.strings = c("", "NA"))
test <- fread("./data/test_ver2.csv",nrows=-1, na.strings = c("", "NA"))

#Save the target variables names
p <- names(train[,grep("^ind_.*_ult1", names(train)),  with = FALSE])
save(p, file = "./files/columns.Rda")

#Introduce a new variable for an easier train/test identification
train[, products := 'train']
test[, products := 'none']
test[, (p) := 0]

# Join train and test in one data table
data <- rbind(train, test)
save(data, file = "./files/data.Rda")

#Remove unused variables
rm(train)
rm(test)
gc()


data[, fecha_dato := as.POSIXct(data[, fecha_dato], format="%Y-%m-%d")]
data[, fecha_alta := as.POSIXct(data[, fecha_alta], format="%Y-%m-%d")]

# Changes the antiguedad to a annual unit
data[, antiguedad := round(data[, antiguedad/12],0)]
data[, 'month' := month(data[, fecha_dato])]
data[, 'alta_month' := month(data[, fecha_alta])]
data[, fecha_alta := NULL]

data[grep('01', data[, segmento]), segmento := 2]
data[grep('02', data[, segmento]), segmento := 0]
data[grep('03', data[, segmento]), segmento := 1]

# All the NAs present in the variable segmento are imputed as 2 (the most common class)
data[is.na(data[,segmento]), segmento :=  2]

# The income targer is imputed according to the province code
data[, renta2 := mean(renta, na.rm = TRUE), by = cod_prov]
data[is.na(renta), renta := renta2]
data[, renta2 := NULL]
# It is applied a log transformation to the income field in order to normalize the distribution (seems to produce no positive effect)
data[, renta := log(data[, renta]+1)]

data[, age := ifelse(age <= 18, 18,  ifelse(age >100, 90, age))]

# The remain NAs values are imputed with -99 
data[is.na(data[, canal_entrada]), canal_entrada := "-99"]
data[is.na(data[, cod_prov]), cod_prov := "-99"]
data[is.na(data[,nomprov]),nomprov := "-99"]
data[is.na(data[,sexo]), sexo := '-99']
data[is.na(data[,indrel_1mes]),indrel_1mes := "-99"]
data[is.na(data[,tiprel_1mes]),tiprel_1mes := "-99"]
data[is.na(conyuemp), conyuemp := '-99']


data <- data[order(ncodpers, fecha_dato),]

save(data, file = "./files/data_cleaned.Rda")