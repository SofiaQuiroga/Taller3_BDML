##########Nuevo intento de modelo de predicción con missings de la mediana 

require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm, biglasso, randomForest)

train_m <- readRDS(url("https://github.com/SofiaQuiroga/Taller3_BDML/blob/main/Data/train_m.rds?raw=true"))

test_m <- readRDS(url("https://github.com/SofiaQuiroga/Taller3_BDML/blob/main/Data/test_m.rds?raw=true"))

YSL <- train_m$price
XSL<- train_m %>% select(surface_covered,bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)

##Modelos un poco más complejos utilizando Random Forest, glmnet y ranger con hiperparámetros

# Ranger
custom_ranger = create.Learner("SL.ranger", params = list(num.trees = 200))
custom_ranger$names

# Random Forest
custom_rf = create.Learner("SL.randomForest",tune = list(mtry = round(c(1, sqrt(4), 3))))
custom_rf$names

# Glmnet
custom_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=3)))
custom_glmnet$names

#Creando la "nueva librería de algoritmos"
sl.lib2 <- c("SL.randomForest", "SL.lm",custom_ranger$names, custom_rf$names, custom_glmnet$names)
sl.lib2

#El Fit
fitY_custom <- SuperLearner(Y = YSL, X = data.frame(XSL),method = "method.NNLS", SL.library = sl.lib2)
fitY_custom

#Predicción
SL_pred <- predict(fitY_custom, newdata= test_m, SLonly=TRUE)$pred
head(SL_pred)

#prueba MAE con el training set
train_m<- train_m %>% mutate(yhat=predict(fitY_custom, newdata= test_m, SLonly=TRUE)$pred)$head(test_m$y_hat)
with(test_m, mean(abs(price-yhat_1))) #MAE

#guardar predicciones
predicciones_sl<- data.frame('property_id' = test_m$property_id, "price" = SL_pred)
colnames(predicciones_sl)[2]<-"price"
write.csv(predicciones_sl, 'prediccion_SL.csv',row.names=FALSE)  



