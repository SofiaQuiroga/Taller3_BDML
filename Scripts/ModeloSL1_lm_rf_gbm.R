require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm)
###### cambiamos los Na de baños pór 0
train2<- train
test2<-test
train2$bathrooms<- ifelse(is.na(train2$bathrooms),0,train$bathrooms)
train2$property_type<- ifelse(train2$property_type=="Apartamento",0,1)

test2$bathrooms<- ifelse(is.na(test2$bathrooms),0,test2$bathrooms)
test2$property_type<- ifelse(test2$property_type=="Apartamento",0,1)


###Modelo Super learners
listWrappers()
variable.names(train2)
#
Ytrain <- train$price
Xtrain<- train2 %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)

sl.models<- c("SL.lm","SL.ranger","SL.gbm")
SLY <- SuperLearner(Y = Ytrain,  X= Xtrain,
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.models)

SLY

#Predecimos
SL1Pred<-predict(SLY,newdata = test2,onlySL = TRUE)
SL1pred1<- data.frame(SL1Pred) 
typeof(SL1pred1)
head(SL1pred1)
SL1pred2<- SL1pred1[,-c(2,3,4)]
SL1pred2<-data.frame(SL1pred2)

##guardamos la base con los predichos
variable.names(test2)
Pred_SL1<- data.frame('property_id' = test2$property_id, "price" = SL1pred2 )
colnames(Pred_SL1)[2]<-"price"
write.csv(Pred_SL1, 'Pred_SL1.csv',row.names=FALSE)       
