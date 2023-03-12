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

###

### Random Forest
require("pacman")
p_load(tidyverse,rpart,caret, randomForest)
#creamos los parametros para el arbol 
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))
ctrl <- trainControl(method = "cv",
                    number = 20,
                    summaryFunction = fiveStats,
                    verbose=FALSE,
                    savePredictions = T)

#creamos el bosque enfocado en la accuracy
forest <- train(price ~ surface_total + rooms + bedrooms + bathrooms + property_type + 
                distancia_parque + distancia_banco + distancia_hospital + distancia_policia + 
                distancia_social + distancia_colegio + parqueadero + social + terraza, 
                data = train_m, 
                method = "rf",
                trControl = ctrl,
)   

forest

# Predecimos 
resultados <- predict(forest, newdata = test_m)
head(resultados)
summary(resultados)

# Guardamos la base con los predichos
Random_Forest <- data.frame('property_id' = test_m$price, 'price' = resultados)

write.csv(Random_Forest, 'Random_Forest.csv',row.names=FALSE)
