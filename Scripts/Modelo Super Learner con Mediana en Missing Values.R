train_m <- train
test_m <- test

# Convertir ceros en missing values 
train_m$surface_total <- ifelse(train_m$surface_total == 0, NA, train_m$surface_total)
test_m$surface_total <- ifelse(test_m$surface_total == 0, NA, test_m$surface_total)

# Definir la categoria de aperamento como 0 y casa como 1
train_m$property_type<- ifelse(train_m$property_type=="Apartamento",0,1)
test_m$property_type<- ifelse(test_m$property_type=="Apartamento",0,1)

# Cambiar missing values por la mediana 
train_m <- train_m %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))
test_m <- test_m %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))

saveRDS(train_m, "train_m.rds")
saveRDS(test_m, "test_m.rds")

### Super Learner 
p_load("SuperLearner")

ySL <- train_m$price # definir variable interes, precios
XSL <- train_m  %>% select(surface_total, rooms, bedrooms, bathrooms, property_type, 
                           distancia_parque, distancia_banco, distancia_hospital, distancia_policia, 
                           distancia_social, distancia_colegio, parqueadero, social, terraza) # definir predictoras

# Definir libreria 
listWrappers()
sl.lib <- c("SL.lm", "SL.ridge", "SL.glm", "SL.ranger") 

# Fit using the SuperLearner package,
fitY <- SuperLearner(Y = ySL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY

# Predicción 
price_SL <- predict(fitY, newdata = test_m, onlySL = TRUE)
price_SL <- data.frame(price_SL) 
price_SL<- price_SL[,-c(2:5)]
price_SL <- data.frame(price_SL) 

resultados <- data.frame("property_id" = test_m$property_id, "price" = price_SL)
colnames(resultados)[2] <- "price"
setwd("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 3")
write.csv(resultados, 'SuperLearner_price.csv',row.names=FALSE) 


### Super Learner 2.0
# Definir libreria 
listWrappers()

custon_ranger = create.Learner("SL.ranger", params = list(num.trees = 500))
custon_ranger$names

custom_rf = create.Learner("SL.randomForest",
                           tune = list(mtry = round(c(1, sqrt(4), 4))))
custom_rf$names

custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=4)))
custon_glmnet$names

sl.lib2 <- c("SL.randomForest", "SL.lm",custon_ranger$names,custom_rf$names, custon_glmnet$names)
sl.lib2

fitY_long <- SuperLearner(Y = ySL, X = data.frame(XSL),
                          method = "method.NNLS", SL.library = sl.lib2)

fitY_long

# Predicción 
price_SL <- predict(fitY, newdata = test_m, onlySL = TRUE)
price_SL <- data.frame(price_SL) 
price_SL<- price_SL[,-c(2:5)]
price_SL <- data.frame(price_SL) 

resultados <- data.frame("property_id" = test_m$property_id, "price" = price_SL)
colnames(resultados)[2] <- "price"
setwd("C:/Users/Sofia/OneDrive - Universidad de los Andes/8. Octavo Semestre/Big Data y Machine Learning/Talleres/Taller 3")
write.csv(resultados, 'SuperLearner_price.csv',row.names=FALSE) 
