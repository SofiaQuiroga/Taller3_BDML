####Otra prueba de modelos : lm, logreg, gbm, ridge, lasso, ranger y random forest
##Super learners
require(pacman)
p_load(SuperLearner,tidyverse,rpart,caret, gbm)

# Review available models.
listWrappers()
variable.names(train2)

##Tratar los missings (de forma diferente: con la mediana)

# Cambio de missing values por la mediana 
train_mediana <- train
train_mediana <- train_mediana %>% mutate(across(where(is.numeric), ~replace_na(., median(., na.rm = TRUE))))


#Primero: lm,logreg,gbm,ridge

YSL <- train$price
XSL<- train_mediana %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)


#sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

sl.lib <- c("SL.lm", "SL.logreg", "SL.gbm",  "SL.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

fitY_1 <- SuperLearner(Y = YSL,  X= data.frame(XSL),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY_!


#Segundo: lm,logreg,gbm

YSL_2 <- train$price
XSL_2<- train2 %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)


#sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

sl.lib <- c("SL.lm", "SL.logreg", "SL.gbm",  "SL.ridge") #lista de los algoritmos a correr

# Fit using the SuperLearner package,

fitY_2 <- SuperLearner(Y = YSL_2,  X= data.frame(XSL_2),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY_2

#Tercero: biglasso, glmnet, lm, gbm

YSL_3 <- train$price
XSL_3<- train2 %>% select(bedrooms,bathrooms,property_type,distancia_parque,distancia_hospital,distancia_policia,distancia_social,distancia_banco,distancia_colegio,parqueadero,social)


#sl.lib <- c("SL.randomForest", "SL.lm") #lista de los algoritmos a correr

sl.lib <- c("SL.lm", "SL.glmnet", "SL.gbm", "SL.biglasso" ) #lista de los algoritmos a correr

# Fit using the SuperLearner package,

fitY_3 <- SuperLearner(Y = YSL_3,  X= data.frame(XSL_3),
                     method = "method.NNLS", # combinación convexa
                     SL.library = sl.lib)

fitY_3


##Modelos un poco más complejos utilizando Random Forest con hiperparámetros

# Customize the defaults for ranger
custom_ranger = create.Learner("SL.ranger", params = list(num.trees = 1000))

# Look at the object.
custom_ranger$names

# Customize the defaults for random forest
custom_rf = create.Learner("SL.randomForest",
                           tune = list(mtry = round(c(1, sqrt(4), 3))))
custom_rf$names

# Customize the defaults for glmnet
custon_glmnet = create.Learner("SL.glmnet", tune = list(alpha = seq(0, 1, length.out=5)))

# Look at the object.
custom_glmnet$names

#Creando la "nueva librería de algoritmos"
sl.lib2 <- c("SL.randomForest", "SL.lm",custom_ranger$names, custom_rf$names, custom_glmnet$names)
sl.lib2


#El Fit

fitY_custom <- SuperLearner(Y = YSL, X = data.frame(XSL),
                          method = "method.NNLS", SL.library = sl.lib2)

fitY_custom





######################################### TRATAR CON LOS MISSSINGS 

test$surface_total <- ifelse(is.na(test$surface_total), 0, test$surface_total)
train$surface_total <- ifelse(is.na(train$surface_total), 0, train$surface_total)


test$descripcion <- gsub(" ", "", test$description)
test$metros <- str_extract(test$descripcion, "[0-9]+m[t[r][ts][t2][etros]?2]")

train$descripcion <- gsub(" ", "", train$description)
train$metros <- str_extract(train$descripcion, "[0-9]+m[t[r][ts][t2][etros]?2]")

# Cambiar los metros que no se encontraron por ceros 
test$metros <- ifelse(is.na(test$metros), 0, test$metros)
train$metros <- ifelse(is.na(train$metros), 0, train$metros)

# Dejar solo el número 
test$metros <- str_replace_all(test$metros, "m[t[r][ts][t2][etros]?2]", "")
train$metros <- str_replace_all(train$metros, "m[t[r][ts][t2][etros]?2]", "")

# Convertir a número
test$metros <- as.numeric(test$metros)
train$metros <- as.numeric(train$metros)

## Unificar variables de supericie total y metros 
test$surface_covered <- ifelse(is.na(test$surface_covered), 0, test$surface_covered)
test$surface_covered <- ifelse(test$surface_covered == 0, test$metros, test$surface_covered)

## Unificar variables de supericie total y metros 
train$surface_covered <- ifelse(is.na(train$surface_covered), 0, train$surface_covered)
train$surface_covered <- ifelse(train$surface_covered == 0, train$metros, train$surface_covered)



###############El vecino más cercano 
install.packages("class")
p_load("VIM")

train_final <- kNN(train, variable=c("surface_covered", "rooms", "bedrooms", "bathrooms"), k=8)

train_final <- kNN(train, variable=c("rooms"), k=8)





