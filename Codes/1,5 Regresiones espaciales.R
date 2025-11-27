rm(list = ls())
graphics.off()
clc <- function() cat("\014") 
clc()


library(tidyverse)
library(data.table)
library(plm)
library(lubridate)
library(fixest)
library(splm)
library(maptools)
library(stargazer)
library(spdep)
####################################
#### CARGAR BASE DE DATOS ##########
####################################

migracion <- fread("datos_migracion.csv", data.table = F)

####################################
#### REGRESIONES ESPACIALES ########
####################################
data(Produc, package = "plm")
data(usaww, package = "splm")
data()
class(Produc)

shape <- readShapePoly("Latam_20.shp") 
plot(shape)
summary(shape)
unique(migracion$country)
class(shape)
shape2 <- subset(shape, COUNTRY !="Cuba"
                 & COUNTRY !="Dominican Republic"
                 & COUNTRY !="Haiti")
migracion2 <- subset(migracion, country !="Cuba"
                     & country !="Dominican Republic"
                     & country !="Haiti")

shape3<- shape2[shape2$COUNTRY %in% migracion2$country,]


pr.nb <- poly2nb(shape3, queen=T, shape2$COUNTRY)
pr.nb
wqueen <- nb2listw(pr.nb, style="W")
rook.fr.st<-listw2mat(nb2listw(pr.nb, style ="W")) 


cent <- coordinates(shape3)
plot(shape2, border="grey", lwd=1.5)
plot(pr.nb,cent, add=T, col="darkred")

glimpse(migracion)
m1 <- spml(
  mean_temperatura~mean_temperatura
           , data = migracion2
           , listw = wqueen
           , lag=T
           , model="pooling"
           )


m1 <- plm(log(urb)~(mean_temperatura)
          , data = migracion
          , model = "pooling")
fespaterr <- spml(log(urb)~(mean_temperatura)
                  , data = migracion2
                  , listw = wqueen,
                  model="within", spatial.error="b", Hess = FALSE)


dim(model.matrix(m1)) 
n_unik(migracion2$country)
