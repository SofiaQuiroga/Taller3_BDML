##################Estadísticas descriptivas 

##Instalamos paquetes 
require("pacman")
p_load("tidyverse","rvest", "stargazer","vtable") ##cargamos los paquetes.

#Estadísticas descriptivas para la base de train (missings con ceros)
sumtable(train2)







#Gráfica de distribución del ingreso (en pesos colombianos)

ggplot(train2, aes(x = Ingtot_hogar)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Ingreso total por hogar (COP)", y = "Cantidad", title="Distribución del Ingreso Total de los Hogares")

#Gráfico de la distribución de pobreza 

#Label variables 

train2$Pobre <- factor(train2$Pobre,
                       levels=c(0,1),
                       labels= c("no", "si"))


ggplot(train2, aes(y=Pobre)) + geom_bar(aes(x = (..count..)/sum(..count..)),
                                        fill = "darkblue") + labs(title ="Distribución de la variable de Pobreza del Hogar", x = "Proporción (%)") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw()

##Uniendo las dos gráficas en una sola

g1<- ggplot(train2, aes(x = Ingtot_hogar)) +
  geom_histogram(fill = "darkblue") +
  theme_bw() +
  labs(x = "Ingreso total por hogar (COP)", y = "Cantidad", title="Distribución del Ingreso Total")


g2<- ggplot(train2, aes(y=Pobre)) + geom_bar(aes(x = (..count..)/sum(..count..)),
                                             fill = "skyblue") + labs(title ="Distribución de Pobreza", x = "Proporción (%)") +
  scale_x_continuous(labels = scales::percent) +
  theme_bw()


grid.arrange(g1, g2, ncol = 2)