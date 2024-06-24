#packages####
#install.packages("dplyr")
#install.packages("stats")
library(stats)
library(dplyr)
library(ggplot2)
#install.packages("ggfortify")
library(ggfortify)
#install.packages("lessR")
library(lessR)
#install.packages("mapdata")
library(mapdata)
#install.packages("ggmap")
library(ggmap)
#install.packages("ggsn")
library(ggsn)
library(sf)
#install.packages("spData")
library(spData)
#install.packages("tmap")
library(tmap)
library(leaflet)
#install.packages("mapview")
library(mapview)
#install.packages("maps")
library(maps)
#install.packages("ggrepel")
library(ggrepel)
library(tidyverse)
library(plotly)
#install.packages("animation")
library(animation)
# Cargamos el paquete gganimate
#install.packages("gganimate")
library(gganimate)
#install.packages("ggthemes")
library(ggthemes)
library(histogram)
library(GGally)
library(caret)
library(lubridate)
library(readxl)
#install.packages("gifski")
library(gifski)
#install.packages("av")
library(av)
#install.packages("revgeo")
library(revgeo)


#codigo####
#previo a subir datos limpiamos columnas que no eran terremotos  
#con el atributo type, eran explosiones, dado que no nos sirven 
#para lo que queremos estudiar

data <- read.csv("C:/Users/rivas/Desktop/Data science/database.csv", sep=";")

#Limpieza de datos####

#aca quitamos columnas que no tenian datos en varias de sus filas
#y otras que consideramos inncesesarias para lo que queremos estudiar

data= data[,-c(7,8,11,12,13,14,15,16,19,20,21)]

view(data)

data <- data|> 
  drop_na() |> 
  unique()


#convert string to date
data$Date2 <- as.Date(data$Date, format="%d-%m-%Y")
data$Year <- as.numeric(format(data$Date2,'%Y'))



table(data$Magnitude, data$Year>1995)



location= c(data$Latitude, data$Longitude)

#Comprobar poder estadistico####

quantile(data$Magnitude)
mean(data$Magnitude)
mean(data$Depth)
hist(data$Depth)
hist((data$Magnitude))
table(data$Magnitude)
mydata= select(data, c(3,4,6,7))
view(mydata)
#wss plot function para seleccionar numero maximo de clusters

wssplot <- function(data, nc = 15, seed=1234) 
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for(i in 2:nc){
      set.seed(seed)
      wss[i] <- sum(kmeans(data,centers = i)$withinss)
    }
    plot(1:nc,wss, type="b", xlab="number of clusters", ylab="within groups sum of squares")
}

wssplot(data)

#plot_ly(alpha=0.9) %>% 
 #   add_histogram(data$Magnitude) %>%
  #  add_histogram(data$Depth)


#plotly::plot_ly(data, x = ~data$Date, y = ~data$Depth, z = ~data$Magnitude, 
 #               color = data$Source , size = 1) |> 
  #plotly::add_markers()


#Trabajo de datos####

set.seed(1000)
s <- data |> 
  slice_sample(n = 3000)
#ggpairs(s, progress = FALSE)

ggplot(data, aes(x = data$Depth, y = data$Magnitude)) +
  geom_point() +
  labs(x = "Profundidad", y = "Magnitud", title = "Distribución de Terremotos por Profundidad y Magnitud")

# Calcular la correlación entre la magnitud y la profundidad
cor(data$Depth, data$Magnitude)

# Calcular el número de terremotos con magnitud mayor a 6
num_large_quakes <- sum(data$Magnitude >= 6)
cat("Número de terremotos con magnitud mayor o igual a 6:", num_large_quakes, "\n")

# Calcular la probabilidad de un terremoto de magnitud mayor a 6 en los datos
prob_large_quake <- num_large_quakes / nrow(data)
cat("Probabilidad de un terremoto de magnitud mayor o igual a 6:", prob_large_quake, "\n")

ggplot (s, aes (s$Depth, s$Magnitude)) + geom_point () + geom_smooth (método = ' lm ')

  
#1996 al 2016

#data.ts=ts(data, start= 1996)

#Probando####

table(data$Year)
quantile(data$Year)
ggplot(data, aes(x = data$Year, y = data$Magnitude)) +
  geom_point() +
  labs(x = "Año", y = "Magnitud", title = "Distribución de Terremotos por Año y Magnitud")

table(data$Latitude)

cor(data$Year,data$Magnitude)


ggplot (s, aes (s$Year, s$Magnitude)) + geom_point () + geom_smooth (método = ' lm ')


quantile(data$Magnitude)


#Geocode####

start <- Sys.time()
#This line do all the reverse geocoding using Photon as a provider
results<-revgeo(longitude=data$Longitude, 
                latitude=data$Latitude, 
                provider = 'photon', output="frame")

end <- Sys.time()

#Ejecutaba un dia entero y nunca terminaba de procesar esas lineas de codigo, escuchaba
#que mi pc hacia mucho ruido y me asustaba, por tanto me veia en la obligacion de cortar 
#la ejecucion para que no le pasara nada. :(



