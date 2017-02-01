# Pick a specific month from the dataset, calculate the lag features and save the resulting dataset
sant_months_load <- function(data, target) {
    target <- as.POSIXct(target, format="%Y-%m-%d")
    load("./files/data_cleaned.Rda")
    data <- data[products != 'none' & (fecha_dato == target | fecha_dato == seq(target, length = 2, by = "-1 months")[2] |
                                           fecha_dato == seq(target, length = 2, by = "-3 months")[2] | 
                                           fecha_dato == seq(target, length = 2, by = "-4 months")[2] |
                                           fecha_dato == seq(target, length = 2, by = "-5 months")[2]) ,(c(ids, var_cat, var_num, products)), with = FALSE]
    print(target)
    data[, inc := length(fecha_dato), by = ncodpers]
    data <- data[inc == 5]
    res <- sapply(p, function(x) paste0('var_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
    data[, (res) := lapply(.SD, function(x) xor(c(0, x[-.N]),x) & x), by = ncodpers, .SDcols = p]
    
    char <- sapply(chars, function(x) paste0('chars_',x))
    data[, (char) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = chars]
    
    
    res <- sapply(p, function(x) paste0('prev_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
    data[, (res) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = p]
    
    res1 <- sapply(p, function(x) paste0('prev1_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
    data[, (res1) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res]
    
    res2 <- sapply(p, function(x) paste0('prev2_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
    data[, (res2) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res1]
    
    res3 <- sapply(p, function(x) paste0('prev3_',paste(unlist(strsplit(x,'_'))[2:(length(unlist(strsplit(x,'_')))-1)], collapse = "_")))
    data[, (res3) := lapply(.SD, function(x) c(0,x[-.N])), by = ncodpers, .SDcols = res2]
    
    data[, activity := Reduce("|", data[,names(data)[grep("^var_", names(data))], with = FALSE])]
    gc()
    data <- data[fecha_dato == target & activity == TRUE]
    gc()
    data[, activity := NULL]
    melt_data <- melt(data, id.vars = c(ids,var_cat, var_num, res, res1, res2, res3, char), measure.vars = names(data)[grep("^var_", names(data))])
    melt_data <- melt_data[value == TRUE]
    melt_data[,value := NULL]
    save(melt_data, file = paste0("./files/data_", year(target),"_", month(target),".Rda"))
    rm(data)
    rm(melt_data)
    gc()
}

