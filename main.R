setwd("C:/Users/tiago_000/Documents/GitHub/Kaggle_Santander_Product_R")


library(data.table)
library(lubridate)
library(xgboost)

load("./files/data_cleaned.Rda")
load("./files/columns.Rda")

# IDs Columns
ids <- c("fecha_dato", "ncodpers")

# Products Columns
products <- names(data)[grep("^ind.*ult1$", names(data))]

# Categorical Columns
var_cat <-  c("ind_empleado", "pais_residencia", "sexo", "canal_entrada", "segmento", "month", "ind_actividad_cliente", "tiprel_1mes", 
              "ind_nuevo", "indrel", "indrel_1mes", "indresi", "indext", "indfall", "tipodom", "conyuemp")
# Numerical Columns
var_num <- c("age", "antiguedad", "renta", "alta_month")

# Columns used in Lag transformations
chars <- c("renta", "segmento", "age", "ind_actividad_cliente", "indfall")

# It is used lags 3 and 5 for products and lag 1 for some other features
test <- data[products == 'none' | fecha_dato == '2016-05-28' | fecha_dato == '2016-03-28' | fecha_dato == '2016-01-28']
test <- test[, products := NULL]

res <- sapply(p, function(x) paste0('var_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
res <- sapply(p, function(x) paste0('prev_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
res1 <- sapply(p, function(x) paste0('prev1_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
res2 <- sapply(p, function(x) paste0('prev2_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
res3 <- sapply(p, function(x) paste0('prev3_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
char <- sapply(chars, function(x) paste0('chars_',x))
test[, (char) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = chars]
test[, (res) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = p]
test[, (res1) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res]
test[, (res2) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res1]
test[, (res3) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res2]
test <- test[fecha_dato == '2016-06-28']
test[, seq_date := as.factor(as.character(fecha_dato))]
test <- test[,(c(var_cat, var_num, res, res1, res2, res3, char, "seq_date")), with = FALSE]
save(test, file = "./files/data_test.Rda")
gc()

#loaded months
sant_months_load(data, '2015-06-28')
sant_months_load(data, '2015-07-28')
sant_months_load(data, '2015-08-28')
sant_months_load(data, '2015-09-28')
sant_months_load(data, '2015-10-28')
sant_months_load(data, '2015-11-28')
sant_months_load(data, '2015-12-28')
sant_months_load(data, '2016-01-28')
sant_months_load(data, '2016-02-28')
sant_months_load(data, '2016-03-28')
sant_months_load(data, '2016-04-28')
sant_months_load(data, '2016-05-28')

load("./files/data_2015_6.Rda")
merge_data <- melt_data
load("./files/data_2015_7.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2015_8.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2015_9.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2015_10.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2015_11.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2015_12.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2016_1.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2016_2.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2016_3.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2016_4.Rda")
merge_data <- rbind(merge_data, melt_data)
load("./files/data_2016_5.Rda")
merge_data <- rbind(merge_data, melt_data)

load("./files/data_test.Rda")

# Character variables are transformed to factors
res <- sapply(merge_data, is.character)
col <- names(merge_data)[res]
merge_data[, (col) := lapply(.SD, factor), .SDcols = col]

res <- sapply(test, is.character)
col <- names(test)[res]
test[, (col) := lapply(.SD, factor), .SDcols = col]

merge_data[, seq_date :=  as.factor(as.character(fecha_dato))]


merge_data[,(ids) := NULL]

merge_data <- data.frame(merge_data)


merge_data <- merge_data[complete.cases(merge_data), ]

levels(merge_data$variable) <- p
merge_data$variable <- droplevels(merge_data$variable)

test <- data.frame(test)
test$variable <- 'null'

# Join train and test data
merge_data <- rbind(merge_data, test)


levels(merge_data$ind_empleado) <- seq_len(length(levels(merge_data$ind_empleado)))
merge_data$ind_empleado <- as.numeric(merge_data$ind_empleado)

levels(merge_data$pais_residencia) <- seq_len(length(levels(merge_data$pais_residencia)))
merge_data$pais_residencia <- as.numeric(merge_data$pais_residencia)

levels(merge_data$sexo) <- seq_len(length(levels(merge_data$sexo)))
merge_data$sexo <- as.numeric(merge_data$sexo)

levels(merge_data$canal_entrada) <- seq_len(length(levels(merge_data$canal_entrada)))
merge_data$canal_entrada <- as.numeric(merge_data$canal_entrada)

levels(merge_data$segmento) <- seq_len(length(levels(merge_data$segmento)))
merge_data$segmento <- as.numeric(merge_data$segmento)

levels(merge_data$tiprel_1mes) <- seq_len(length(levels(merge_data$tiprel_1mes)))
merge_data$tiprel_1mes <- as.numeric(merge_data$tiprel_1mes)

levels(merge_data$indrel_1mes) <- seq_len(length(levels(merge_data$indrel_1mes)))
merge_data$indrel_1mes <- as.numeric(merge_data$indrel_1mes)

levels(merge_data$indresi) <- seq_len(length(levels(merge_data$indresi)))
merge_data$indresi <- as.numeric(merge_data$indresi)

levels(merge_data$indext) <- seq_len(length(levels(merge_data$indext)))
merge_data$indext <- as.numeric(merge_data$indext)

levels(merge_data$indfall) <- seq_len(length(levels(merge_data$indfall)))
merge_data$indfall <- as.numeric(merge_data$indfall)

levels(merge_data$conyuemp) <- seq_len(length(levels(merge_data$conyuemp)))
merge_data$conyuemp <- as.numeric(merge_data$conyuemp)

levels(merge_data$seq_date) <- seq_len(length(levels(merge_data$seq_date)))
merge_data$seq_date <- as.numeric(merge_data$seq_date)

rm(melt_data)
gc()


test <- merge_data[merge_data$variable == 'null', ]

# Drop the 2 categories less used
merge_data <- merge_data[merge_data$variable != 'null' & !(merge_data$variable %in% c("ind_ahor_fin_ult1", "ind_aval_fin_ult1")), ]
merge_data$variable <- droplevels(merge_data$variable)

train_x <- merge_data[, !(names(merge_data) %in% c("variable", "seq_date"))]

y_train <-merge_data$variable


labels <- as.numeric(merge_data$variable)-1
xgb_train <- xgb.DMatrix(model.matrix(~., data = train_x),
                         label=labels, missing=NaN)

n_cl <- length(levels(y_train))-1

length(levels(y_train))

# history <- xgb.cv(data = xgb_train, nround=200, nthread = 6, nfold = 3, 
#                   max.depth =6, eta = 0.1, colsample_bytree = 0.6, num_class = 22,  objective = "multi:softprob", eval_metric = "mlogloss",booster = "gbtree")

param<-list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    booster = "gbtree",
    max_depth = 6,
    eta = 0.1,
    colsample_bytree = 0.7,
    min_child_weight=1,
    num_class = 22
)

Training <-
    xgb.train(params = param,
              data = xgb_train,
              nrounds = 150,
              watchlist = list(train = xgb_train),
              verbose = TRUE,
              print_every_n = 50,
              nthread = 6)




xgb_test <- xgb.DMatrix(model.matrix(~., data = test[, !(names(test) %in% c("variable"))]))

xgb_pred <- predict(Training, newdata=xgb_test, missing=NaN)

xgb_pred <- matrix(xgb_pred, ncol = 22, byrow = TRUE)
prediction <- data.frame(xgb_pred)
names(prediction) <- levels(merge_data$variable)[1:22]

# # Eliminate class probabilities aquired last month
load("./files/data_cleaned.Rda")
data <- data[fecha_dato == '2016-05-28' |  fecha_dato == '2016-06-28']
data[, products0 := do.call(paste, c(data[,p, with = FALSE], sep = ""))]
data[, products1 := lapply(.SD, function(x) c(-1, x[-.N])), by = ncodpers, .SDcols = "products0"]
pred4 <- data.frame(data[products == 'none', c("ncodpers","products1", "products"), with = FALSE])
pred4 <- cbind(pred4, prediction)

func1 <- function(x){
    res <- !as.logical(as.numeric(unlist(strsplit(x["products1"], ''))))
    res1 <- as.numeric(do.call(c,list(x[p])))
    names(res1) <- p
    res1 <- paste(names(sort(res1[res], decreasing = TRUE)[1:7]), collapse = ' ')
}

pred5 <- apply(pred4, 1, func1)

pred5 <- data.frame(ncodpers = pred4$ncodpers, added_products = pred5)

write.csv(pred5, "./predictions/sub.csv", row.names=FALSE)

