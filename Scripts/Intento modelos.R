####Otra prueba de modelos: lm, logreg, gbm, ridge, lasso, ranger y random forest
require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm, biglasso, randomForest)

# Revisión de modelos
listWrappers()
variable.names(train2)

#Con la base que tenemo, cambiamos NA a 0
train_1 <- train
test_1 <- test

train_1$bathrooms<- ifelse(is.na(train_1$bathrooms),0,train_1$bathrooms)
train_1$property_type<- ifelse(train_1$property_type=="Apartamento",0,1)

test_1$bathrooms<- ifelse(is.na(test_1$bathrooms),0,test_1$bathrooms)
test_1$property_type<- ifelse(test_1$property_type=="Apartamento",0,1)

YSL <- train_1$price
XSL<- train_1 %>% select(surface_covered,bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)
sl.lib <- c("SL.lm", "SL.gbm",  "SL.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

fitY_1 <- SuperLearner(Y = YSL,  X= data.frame(XSL),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.lib)
fitY_1
##Coeficiente de gbm es igual a 1, los demás son cero

##Guardo la predicción

SL1_pred_1 <- predict(fitY_1, newdata= test_m, SLonly=TRUE)$pred
head(SL1_pred_1)

#prueba MAE con el training set
train_1<- train_1 %>% mutate(yhat_1=predict(fitY_1, newdata= test_m, SLonly=TRUE)$pred)$head(test$y_hat_1)
with(test_m, mean(abs(price-yhat_1))) #MAE

#guardar predicciones
predicciones_sl_1<- data.frame('property_id' = test_m$property_id, "price" = SL1_pred)
colnames(predicciones_sl_1)[2]<-"price"
write.csv(predicciones_sl1, 'prediccion_SL_1.csv',row.names=FALSE)    




#Segundo: lm,glmnet,gbm

YSL_2 <- train_1$price
XSL_2<- train_1 %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)

#sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

sl.lib <- c("SL.ranger", "SL.lm", "SL.glmnet", "SL.gbm",  "SL.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

fitY <- SuperLearner(Y = YSL_2,  X= data.frame(XSL_2),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.lib)
fitY_2


#Tercero: glmnet, lm, gbm
YSL_3 <- train_1$price
XSL_3<- train_1 %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)

sl.lib <- c("SL.lm", "SL.glmnet", "SL.gbm") #lista de los algoritmos a correr

# Fit using the SuperLearner package,
fitY_3 <- SuperLearner(Y = YSL_3,  X= data.frame(XSL_3),
                       method = "method.NNLS", # combinación convexa
                       SL.library = sl.lib)
fitY_3

#Predicción
SL1_pred <- predict(fitY_3, newdata= test_m, SLonly=TRUE)$pred
head(SL1_pred)

#prueba MAE con el training set
train_m1<- train_1 %>% mutate(yhat_1=predict(fitY_3, newdata= test_1, SLonly=TRUE)$pred)$head(test$y_hat_1)
with(test_m1, mean(abs(price-yhat_1))) #MAE

#guardar predicciones
predicciones_sl1<- data.frame('property_id' = test_m$property_id, "price" = SL1_pred)
colnames(predicciones_sl1)[2]<-"price"
write.csv(predicciones_sl1, 'prediccion_SL1.csv',row.names=FALSE)    


