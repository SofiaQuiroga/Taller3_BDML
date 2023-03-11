#Modelos de predicción
rm(list=ls())
test <- read.csv("/Users/saratorres/Documents/GitHub/Taller3_BDML/Data/test.csv")
train <- read.csv("/Users/saratorres/Documents/GitHub/Taller3_BDML/Data/train.csv")

#cargamos los paquetes
require(pacman)
p_load("SuperLearner","tidyverse","rpart","caret", "gbm", "glmnet")

#cambio de NA a 0
train_m <- train
test_m <- test

train_m$bathrooms<- ifelse(is.na(train_m$bathrooms),0,train_m$bathrooms)
train_m$property_type<- ifelse(train_m$property_type=="Apartamento",0,1)

test_m$bathrooms<- ifelse(is.na(test_m$bathrooms),0,test_m$bathrooms)
test_m$property_type<- ifelse(test_m$property_type=="Apartamento",0,1)

listWrappers()
variable.names(train_m)

#Superlearners
p_load("SuperLearner")
listWrappers()

ySL2 <- train_m$price
xSL2 <- train_m %>% select(bedrooms, distancia_parque, bathrooms, distancia_colegio, distancia_social, terraza, parqueadero)
sl.lib2 <- c("SL.gbm",  "SL.glm", "SL.lm")
fitY2 <- SuperLearner(Y= ySL2, X= data.frame(xSL2), method= "method.NNLS", SL.librar= sl.lib2)
fitY2
#El alpha de gbm es 1

SL2pred <- predict(fitY2, newdata= test_m, SLonly=TRUE)$pred
head(SL2pred)
#prueba MAE con el training set
train_m1<- train_m %>% mutate(yhat_1=predict(fitY2, newdata= train_m, SLonly=TRUE)$pred)
with(train_m1, mean(abs(price-yhat_1)))

#guardar predicciones
predicciones_sl2<- data.frame('property_id' = test_m$property_id, "price" = SL2pred )
colnames(predicciones_sl2)[2]<-"price"
write.csv(predicciones_sl2, 'prediccion_SL2.csv',row.names=FALSE)    

