##################Estadísticas descriptivas 

##Instalamos paquetes 
require("pacman")
p_load("tidyverse","rvest", "stargazer","vtable") ##cargamos los paquetes.

#Estadísticas descriptivas para la base de train (con Na)

#Ya contenidas en la base: rooms, bedrooms, bathrooms, surface covered
predictores_train <- train %>% select(price, surface_covered, bedrooms, rooms, bathrooms)
sumtable(predictores_train)

#Distancia (extraídas a partir del OSM)
predictores_OSM <- train %>% select(distancia_parque, distancia_hospital, distancia_colegio, distancia_social, distancia_policia, distancia_banco)
sumtable(predictores_OSM)

#Descipción
preditores_descripcion <- train %>% select(terraza, parqueadero, social)
sumtable(preditores_descripcion)


#Tabla de correlacion
cor(train)

