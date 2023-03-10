####Otra prueba de modelos : lm, logreg, gbm, ridge, lasso, ranger y random forest
##Super learners
require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm, biglasso, randomForest)

# Review available models.
listWrappers()
variable.names(train2)

##Tratar los missings (de forma diferente: con la mediana)

# Cambio de missing values por la mediana (incluyendo el surface covered)
train_mediana <- train
#Reemplazo los surface_covered en 0 por NA para luego poner la mediana
library(dplyr) 
train_mediana <- train_mediana %>% mutate_at(c('surface_covered'), ~na_if(., 0))
train_mediana <- train_mediana %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))


#Con la base que tenemos
#cambiamos NA a 0
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

##################

##Modelos un poco más complejos utilizando Random Forest, glmnet y ranger con hiperparámetros

# Customize the defaults for ranger
custom_ranger = create.Learner("SL.ranger", params = list(num.trees = 1000))

# Look at the object.
custom_ranger$names

# Customize the defaults for random forest
custom_rf = create.Learner("SL.randomForest",tune = list(mtry = round(c(1, sqrt(4), 3))))
custom_rf$names

# Customize the defaults for glmnet
custom_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))

# Look at the object.
custom_glmnet$names

#Creando la "nueva librería de algoritmos"
sl.lib2 <- c("SL.randomForest", "SL.lm",custom_ranger$names, custom_rf$names, custom_glmnet$names)
sl.lib2

#El Fit
fitY_custom <- SuperLearner(Y = YSL, X = data.frame(XSL),method = "method.NNLS", SL.library = sl.lib2)
fitY_custom

#Predicción
SL2_pred <- predict(fitY_custom, newdata= test_m, SLonly=TRUE)$pred
head(SL2_pred)

#prueba MAE con el training set
train_m2<- train_1 %>% mutate(yhat_2=predict(fitY_custom, newdata= test_1, SLonly=TRUE)$pred)$head(test_m$y_hat_2)
with(test_1, mean(abs(price-yhat_2))) #MAE

#guardar predicciones
predicciones_sl2<- data.frame('property_id' = test_m$property_id, "price" = SL2_pred)
colnames(predicciones_sl2)[2]<-"price"
write.csv(predicciones_sl2, 'prediccion_SL2.csv',row.names=FALSE)    

