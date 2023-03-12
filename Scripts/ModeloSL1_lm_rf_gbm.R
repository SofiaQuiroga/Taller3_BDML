require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm, glmnet)
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


################### nuevas varaibles y mas complejidad ##################
#creamos los parametros de los modelos y el Super learner

custom_ranger = create.Learner("SL.ranger", params = list(num.trees = c(200,300),mtry=c(2,5,7)))
custom_ranger$names
custom_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, by=0.2)))
sl.models2 <- c(custon_ranger$names,custom_glmnet$names,"SL.lm","SL.gbm")                              
sl.models2

Ytrain2 <- train2$price
names(test2)
Xtrain2<- train2 %>% select(surface_total,bedrooms,property_type,distancia_parque,distancia_hospital,
                           distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,
                           social,terraza,estrato)

#corremos el modelo
SLY2 <- SuperLearner(Y = Ytrain2,  X= Xtrain2,
                    method = "method.NNLS", # combinación convexa
                    SL.library = sl.models2)

SLY2
#Predecimos
SL2Pred<-predict(SLY2,newdata = test2,onlySL = TRUE)
SL2pred1<- data.frame(SL2Pred) 
typeof(SL2pred1)
head(SL2pred1)
SL2pred2<- SL2pred1[,-c(2:10)]
SL2pred2<-data.frame(SL2pred2)

##guardamos la base con los predichos
variable.names(test2)
Pred_SL2<- data.frame('property_id' = test2$property_id, "price" = SL2pred2 )
colnames(Pred_SL2)[2]<-"price"
write.csv(Pred_SL2, 'Pred_SL2.csv',row.names=FALSE) 

