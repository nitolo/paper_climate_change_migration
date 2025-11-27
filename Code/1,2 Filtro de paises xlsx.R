rm(list = ls())
graphics.off()
clc <- function() cat("\014") 
clc()


library(tidyverse)
library(readxl)
library(data.table)


datos <- read_excel("~/ECONOMIA/NOVENO/Análisis espacial/Paper_spatial/Datos en bruto/Datos panel (primera version).xlsx")
glimpse(datos)
datos$year <- as.integer(datos$year)
glimpse(datos)

comercio <- fread("comercio.csv", data.table = F) %>% 
  select(-V1) %>% 
  rename(country=Entity, code=Code, year=Year, trade=`Trade (% of GDP)`)

datos_com <- left_join(datos, comercio, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)

# urbanizacion
urbanizacion <- fread("urbanizacion.csv", data.table = F)%>%
  select(-V1) %>%
  rename(country=Entity, code=Code, year=Year
         , urb=`Urban population (% of total population)`)

glimpse(urbanizacion)

datos_com <- left_join(datos_com, urbanizacion, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)

# ciudad
ciudad <- fread("ciudad.csv", data.table = F)
glimpse(ciudad)

ciudad <- fread("ciudad.csv", data.table = F)%>%
  select(-V1) %>%
  rename(country=Entity, code=Code, year=Year
         , city=`Population in the largest city (% of urban population)`)

glimpse(ciudad)

datos_com <- left_join(datos_com, ciudad, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)

# barrios_marginados
barrios_marginados <- fread("barrios_marginados.csv", data.table = F)
glimpse(barrios_marginados)

barrios_marginados <- fread("barrios_marginados.csv", data.table = F)%>%
  select(-V1) %>%
  rename(country=Entity, code=Code, year=Year
         , slums=`Population living in slums (% of urban population)`)

glimpse(barrios_marginados)

datos_com <- left_join(datos_com, barrios_marginados, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)


# aglomeracion
aglomeracion <- fread("aglomeracion.csv", data.table = F)
glimpse(aglomeracion)

aglomeracion <- fread("aglomeracion.csv", data.table = F)%>%
  select(-V1) %>%
  rename(country=Entity, code=Code, year=Year
         , aglo=`Population in urban agglomerations of more than 1 million (% of total population)`)

glimpse(aglomeracion)

datos_com <- left_join(datos_com, aglomeracion, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)

# gdp
gdp <- fread("gdp.csv", data.table = F)
glimpse(gdp)

gdp <- fread("gdp.csv", data.table = F)%>%
  select(-V1) %>%
  rename(country=Entity, code=Code, year=Year
         , gdp=`GDP per capita (constant 2015 US$)`)

glimpse(gdp)

datos_com <- left_join(datos_com, gdp, by=c("country", "code", "year"))

glimpse(datos_com)
unique(datos_com$country)

# poblacion
poblacion <- fread("poblacion.csv", data.table = F)
glimpse(poblacion)

poblacion <- fread("poblacion.csv", data.table = F)%>%
  select(`Country name`, Year, Population, `Population aged 15 to 64 years`) %>%
  rename(country=`Country name`, pop=Population, year=Year
         , pop_work=`Population aged 15 to 64 years`)

glimpse(poblacion)

datos_com <- left_join(datos_com, poblacion, by=c("country", "year"))

glimpse(datos_com)
unique(datos_com$country)

# democracia
democracia <- fread("democracia.csv", data.table = F)
glimpse(democracia)

democracia <- fread("democracia.csv", data.table = F)%>%
  select(Entity, Year, electdem_vdem_owid, Code) %>%
  rename(country=Entity, code=Code, year=Year
         , dem=electdem_vdem_owid)

glimpse(democracia)

datos_com <- left_join(datos_com, democracia, by=c("country","code", "year"))

glimpse(datos_com)
unique(datos_com$country)


################################################
######### exportar datos #######################
################################################

write.csv(datos_com, "datos_com.csv")


