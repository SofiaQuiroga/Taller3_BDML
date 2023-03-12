### Taller 3
## Clean the workspace
rm(list=ls())

## Cargar paquetes
require("pacman")
# Cargar las librerías listadas e instalarlas en caso de ser necesario
p_load(tidyverse, # Manipular dataframes
       rstudioapi, # Get the location of this script
       rio, # Import data easily
       plotly, # Gráficos interactivos
       leaflet, # Mapas interactivos
       rgeos, # Calcular centroides de un poligono
       tmaptools, # geocode_OSM()
       sf, # Leer/escribir/manipular datos espaciales
       stargazer, # Report LM models
       osmdata) # Get OSM's data 

## Cargar datos 
test <- read.csv(url("https://github.com/SofiaQuiroga/Taller3_BDML/blob/main/Data/test.csv?raw=true"))
train <- read.csv(url("https://github.com/SofiaQuiroga/Taller3_BDML/blob/main/Data/train.csv?raw=true"))

## Definir el espacio de Chapinero 
limites <- getbb("Chapinero Bogota Colombia") # da información espacial del mundo especificado

## Observamos la primera visualización. Graficar las longitudes y latitutes
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

### Feature Engineering. Primera Variable: Distancia al parque más cercano
available_tags("leisure")

# Obtener información de los parques en Bogotá
parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park")

parques_sf <- osmdata_sf(parques) # convertir a datos esfericos

parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos del parque

centroide_parque <- gCentroid(as(parques_geometria$geometry, "Spatial"), byid = T) # centro del parque 

## Calcular la distancia de cada vivienda al centroide de cada parque
# Objetos satelitales
train_sf <- st_as_sf(train, coords = c("lon", "lat"))
test_sf <- st_as_sf(test, coords = c("lon", "lat")) 

# Dimensiones de la tierra para hacer proyección en el plano
st_crs(train_sf) <- 4326 
st_crs(test_sf) <- 4326 

centroide_parque_sf <- st_as_sf(centroide_parque, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada parque
dist_matrix_parque_train <- st_distance(x = train_sf, y = centroide_parque_sf)
dist_matrix_parque_test <- st_distance(x = test_sf, y = centroide_parque_sf)

## Encontramos la distancia mínima a un parque (parque más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_parque_train, 1, min) 
train$distancia_parque <- dist_min_train
train_sf$distancia_parque <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_parque_test, 1, min) 
test$distancia_parque <- dist_min_test
test_sf$distancia_parque <- dist_min_test

### Feature Engineering. Segunda Variable: Distancia al hospital más cercano
available_tags("amenity")

# Obtener información de los hospitales en Bogotá
hospitales <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "hospital")

hospitales_sf <- osmdata_sf(hospitales) # convertir a datos esfericos

hospitales_geometria <- hospitales_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos de hospitales

centroide_hospital <- gCentroid(as(hospitales_geometria$geometry, "Spatial"), byid = T) # centro del parque 

## Calcular la distancia de cada vivienda al centroide de cada hospital
centroide_hospital_sf <- st_as_sf(centroide_hospital, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada hospital
dist_matrix_hospital_train <- st_distance(x = train_sf, y = centroide_hospital_sf)
dist_matrix_hospital_test <- st_distance(x = test_sf, y = centroide_hospital_sf)

## Encontramos la distancia mínima a un hospital (hospital más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_hospital_train, 1, min) 
train$distancia_hospital <- dist_min_train
train_sf$distancia_hospital <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_hospital_test, 1, min) 
test$distancia_hospital <- dist_min_test
test_sf$distancia_hospital <- dist_min_test


### Feature Engineering. Tercera Variable: Distancia a la estación de polícia más cercana 
available_tags("amenity")

# Obtener información de las estaciones de polícia en Bogotá
policia <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "police")

policia_sf <- osmdata_sf(policia) # convertir a datos esfericos

policia_geometria <- policia_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos de las estaciones de policia

centroide_policia <- gCentroid(as(policia_geometria$geometry, "Spatial"), byid = T) # centro de la estación  

## Calcular la distancia de cada vivienda al centroide de cada estación de polícia
centroide_policia_sf <- st_as_sf(centroide_policia, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada a la estación de polícia
dist_matrix_policia_train <- st_distance(x = train_sf, y = centroide_policia_sf)
dist_matrix_policia_test <- st_distance(x = test_sf, y = centroide_policia_sf)

## Encontramos la distancia mínima a una estación de polícia (estación más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_policia_train, 1, min) 
train$distancia_policia <- dist_min_train
train_sf$distancia_policia <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_policia_test, 1, min) 
test$distancia_policia <- dist_min_test
test_sf$distancia_policia <- dist_min_test


### Feature Engineering. Cuarta Variable: Distancia al social centre más cercano 
available_tags("amenity")

# Obtener información de los social centre en Bogotá
social <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "social_centre")

social_sf <- osmdata_sf(social) # convertir a datos esfericos

social_geometria <- social_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos de los social centre

centroide_social <- gCentroid(as(social_geometria$geometry, "Spatial"), byid = T) # centro del social centre 

## Calcular la distancia de cada vivienda al centroide de cada social centre
centroide_social_sf <- st_as_sf(centroide_social, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada social centre
dist_matrix_social_train <- st_distance(x = train_sf, y = centroide_social_sf)
dist_matrix_social_test <- st_distance(x = test_sf, y = centroide_social_sf)

## Encontramos la distancia mínima a un social centre (social centre más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_social_train, 1, min) 
train$distancia_social <- dist_min_train
train_sf$distancia_social <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_social_test, 1, min) 
test$distancia_social <- dist_min_test
test_sf$distancia_social <- dist_min_test


### Feature Engineering. Quinta Variable: Distancia al banco/cajero más cercano 
available_tags("amenity")

# Obtener información de los bancos en Bogotá
banco <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "bank")

banco_sf <- osmdata_sf(banco) # convertir a datos esfericos

banco_geometria <- banco_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos de los bancos

centroide_banco <- gCentroid(as(banco_geometria$geometry, "Spatial"), byid = T) # centro del banco

## Calcular la distancia de cada vivienda al centroide de cada banco
centroide_banco_sf <- st_as_sf(centroide_banco, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada banco
dist_matrix_banco_train <- st_distance(x = train_sf, y = centroide_banco_sf)
dist_matrix_banco_test <- st_distance(x = test_sf, y = centroide_banco_sf)

## Encontramos la distancia mínima a un banco (banco más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_banco_train, 1, min) 
train$distancia_banco <- dist_min_train
train_sf$distancia_banco <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_banco_test, 1, min) 
test$distancia_banco <- dist_min_test
test_sf$distancia_banco <- dist_min_test


### Feature Engineering. Sexta Variable: Distancia al colegio más cercano 
available_tags("amenity")

# Obtener información de los colegios en Bogotá
colegio <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "amenity" , value = "school")

colegio_sf <- osmdata_sf(colegio) # convertir a datos esfericos

colegio_geometria <- colegio_sf$osm_polygons %>% 
  select(osm_id, name) # traer poligonos de los colegios

centroide_colegio <- gCentroid(as(colegio_geometria$geometry, "Spatial"), byid = T) # centro del colegio

## Calcular la distancia de cada vivienda al centroide de cada colegio
centroide_colegio_sf <- st_as_sf(centroide_colegio, coords = c("x", "y"))

# Calcular distancias de cada vivienda a cada colegio
dist_matrix_colegio_train <- st_distance(x = train_sf, y = centroide_colegio_sf)
dist_matrix_colegio_test <- st_distance(x = test_sf, y = centroide_colegio_sf)

## Encontramos la distancia mínima a un colegio (colegio más cercano). 
# Train 
dist_min_train <- apply(dist_matrix_colegio_train, 1, min) 
train$distancia_colegio <- dist_min_train
train_sf$distancia_colegio <- dist_min_train

# Test 
dist_min_test <- apply(dist_matrix_colegio_test, 1, min) 
test$distancia_colegio <- dist_min_test
test_sf$distancia_colegio <- dist_min_test


### Feature Engineering. Palabras más importantes descripción 
p_load("pdftools", "tokenizers", "stopwords", "SnowballC", "wordcloud")

## Normalización del texto 
# Todo en minuscula
test$title <- tolower(test$title)
test$description <- tolower(test$description)

train$title <- tolower(train$title)
train$description <- tolower(train$description)

# Eliminamos tildes
test$title <- iconv(test$title, from = "UTF-8", to = "ASCII//TRANSLIT")
test$description <- iconv(test$description, from = "UTF-8", to = "ASCII//TRANSLIT")

train$title <- iconv(train$title, from = "UTF-8", to = "ASCII//TRANSLIT")
train$description <- iconv(train$description, from = "UTF-8", to = "ASCII//TRANSLIT")

# Eliminamos caracteres especiales. Remplaza todo lo que no es alfanumericos por un espacio
test$title <- str_replace_all(test$title, "[^[:alnum:]]", " ")
test$description <- str_replace_all(test$description, "[^[:alnum:]]", " ")

train$title <- str_replace_all(train$title, "[^[:alnum:]]", " ")
train$description <- str_replace_all(train$description, "[^[:alnum:]]", " ")

# Eliminamos espacios extras
test$title <- gsub("\\s+", " ", str_trim(test$title))
test$description <- gsub("\\s+", " ", str_trim(test$description))

train$title <- gsub("\\s+", " ", str_trim(train$title))
train$description <- gsub("\\s+", " ", str_trim(train$description))

## Tokenización 
test_descripcion_tokenizado <- tokenize_words(test$description)

## Eliminar stopwords 
# Descargamos la lista de las stopwords en español de dos fuentes diferentes y las combinamos
lista_palabras1 <- stopwords(language = "es", source = "snowball")
lista_palabras2 <- stopwords(language = "es", source = "nltk")
lista_palabras3 <- c("Casa", "casa", "Apartamento", "apartamento")
lista_palabras <- union(lista_palabras1, lista_palabras2)
lista_palabras <- union(lista_palabras, lista_palabras3)
lista_palabras

test_descripcion_tokenizado <- test_descripcion_tokenizado[[1]]
test_descripcion_tokenizado <- setdiff(test_descripcion_tokenizado, lista_palabras)

test_descripcion_tokenizado <- wordStem(test_descripcion_tokenizado, "spanish")
test_descripcion_tokenizado[1:500]

# Nota: Revisar este enlace para guardar la nubede palabras funciona! 
#setwd("https://github.com/SofiaQuiroga/Taller3_BDML/tree/main/Views")
frecuencia <- test_descripcion_tokenizado %>%
  table() %>%
  data.frame() %>%
  rename("Palabra" = ".") %>%
  arrange(desc(Freq))

set.seed(3312) 
png(filename = "wordcloud.png", width = 800, height = 800)
wordcloud(words = frecuencia$Palabra, freq = frecuencia$Freq, min.freq = 1,
          max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))

dev.off()

## A partir de las palabras de más usadas en la descripción, se crean las variables parqueadero, social y terraza
## La variable parqueadero es dummy, y será igual a 1 si la vivienda tiene parqueadero, y cero de lo contrario 
## La variable social es dummy, y será igual a 1 si la vivienda tiene espacios sociales, y cero de lo contrario 
# La variable terraza es dummy, y será igual a 1 si la vivienda tiene terrasa, y cero de lo contrario

# Variable parqueadero 
test$descripcion_tokenizado <- tokenize_words(test$description)
test$parqueadero <- as.integer(as.logical(grepl(paste(c("parqueadero?", "garaje?"), collapse = "|"), test$descripcion_tokenizado)))

train$descripcion_tokenizado <- tokenize_words(train$description)
train$parqueadero <- as.integer(as.logical(grepl(paste(c("parqueadero?", "garaje?"), collapse = "|"), train$descripcion_tokenizado)))

# Variable social
test$social <- as.integer(as.logical(grepl("socia(l|es)", test$descripcion_tokenizado)))
train$social <- as.integer(as.logical(grepl("socia(l|es)", train$descripcion_tokenizado)))

# Variable terraza 
test$terraza <- as.integer(as.logical(grepl(paste(c("terraza?", "balcon?"), collapse = "|"), test$descripcion_tokenizado)))
train$terraza <- as.integer(as.logical(grepl(paste(c("terraza?", "balcon?"), collapse = "|"), train$descripcion_tokenizado)))
####

## Obtener los metros cuadrados de la descripción 
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
test$surface_total <- ifelse(is.na(test$surface_total), 0, test$surface_total)
test$surface_total <- ifelse(test$surface_total == 0, test$metros, test$surface_total)

train$surface_total <- ifelse(is.na(train$surface_total), 0, train$surface_total)
train$surface_total <- ifelse(train$surface_total == 0, train$metros, train$surface_total)

### Arreglar missing values por el vecino cercano 
p_load(VIM)
train$surface_total <- as.integer(train$surface_total)
train_final <- kNN(train, variable=c("surface_covered"), k=8)

#####Crear la variable de estrato por localidades.
##����tener cuidado con esta varible, ya que al variar muy poco puede generar ruido en el modelo, voy a hacer pruebas con ella!!!!!!!. 
##Creamos los datos de las localidades 

localidades<- st_read("https://github.com/SofiaQuiroga/Taller3_BDML/raw/main/Data/poligonos-localidades.geojson")
Sf_localidades_train<- st_join(train_sf, localidades)
Sf_localidades_test<- st_join(test_sf, localidades)

localidad_names <- localidades$Nombre.de.la.localidad

#creamos una funci�n que toma el valor de 1 si las coordenadas estan dentro de la localidad, 0 de lo contrario
is_in_localidad <- function(location, localidad) {
  if (location$Nombre.de.la.localidad == localidad) {
    return(1)
  } else {
    return(0)
  }
}
#usamos un loop para crear las varaibles de todas las localidades
for (localidad in localidad_names) {
  Sf_localidades_train[[localidad]] <- apply(Sf_localidades_train, 1, function(x) is_in_localidad(x, localidad))
}

for (localidad in localidad_names) {
  Sf_localidades_test[[localidad]] <- apply(Sf_localidades_test, 1, function(x) is_in_localidad(x, localidad))
}
#colocamos los estratos a cada localidad
Sf_localidades_train<-data.frame(Sf_localidades_train)
Sf_localidades_train$estrato<-0
names(Sf_localidades_train)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$CIUDAD.BOLIVAR==1 |Sf_localidades_train$USME==1 , 1, Sf_localidades_train$estrato)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$BOSA ==1 |Sf_localidades_train$SAN.CRISTOBAL ==1|Sf_localidades_train$TUNJUELITO ==1|Sf_localidades_train$RAFAEL.URIBE.URIBE ==1 , 2, Sf_localidades_train$estrato)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$KENNEDY ==1 | Sf_localidades_train$ENGATIVA ==1 | Sf_localidades_train$PUENTE.ARANDA ==1 |Sf_localidades_train$ANTONIO.NARI�O ==1|Sf_localidades_train$LOS.MARTIRES ==1 
                                       |Sf_localidades_train$CANDELARIA ==1 |Sf_localidades_train$SANTA.FE ==1|Sf_localidades_train$BARRIOS.UNIDOS ==1 , 3, Sf_localidades_train$estrato)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$TEUSAQUILLO ==1 | Sf_localidades_train$SUBA ==1 | Sf_localidades_train$FONTIBON ==1, 4, Sf_localidades_train$estrato)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$USAQUEN ==1, 5, Sf_localidades_train$estrato)
Sf_localidades_train$estrato <- ifelse(Sf_localidades_train$CHAPINERO ==1, 6, Sf_localidades_train$estrato)

#
Sf_localidades_test<-data.frame(Sf_localidades_test)
Sf_localidades_test$estrato<-0
names(Sf_localidades_test)
Sf_localidades_test$estrato <- ifelse(Sf_localidades_test$KENNEDY ==1 | Sf_localidades_test$ENGATIVA ==1 | Sf_localidades_test$PUENTE.ARANDA ==1 |Sf_localidades_test$ANTONIO.NARI�O ==1|Sf_localidades_test$LOS.MARTIRES ==1 
                                      |Sf_localidades_test$CANDELARIA ==1 |Sf_localidades_test$SANTA.FE ==1|Sf_localidades_test$BARRIOS.UNIDOS ==1 , 3, Sf_localidades_test$estrato)
Sf_localidades_test$estrato <- ifelse(Sf_localidades_test$USAQUEN ==1, 5, Sf_localidades_test$estrato)
Sf_localidades_test$estrato <- ifelse(Sf_localidades_test$CHAPINERO ==1, 6, Sf_localidades_test$estrato)
#Pasamos los estratos a la base normal
train$estrato<- Sf_localidades_train$estrato
test$estrato <- Sf_localidades_test$estrato


############################################
                      
                       