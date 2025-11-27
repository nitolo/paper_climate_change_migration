rm(list = ls())
graphics.off()
clc <- function() cat("\014") 
clc()


library(tidyverse); library(data.table); library(plm); library(lubridate)
library(openxlsx)

##############################################
########## CARGAR PASES DE DATOS #############
##############################################

clima <- fread("datos_clima.csv", data.table = F)
glimpse(clima)

otros <- fread("datos_com.csv", data.table = F)
glimpse(otros)


datos <- left_join(otros, clima, by=c("country","code", "year")) %>%
  select(-V1.x, -V1.y) %>% filter(country!="Latin America and Caribbean")
glimpse(datos)

write.csv(datos, "datos_migracion.csv", row.names = F)
write.xlsx(datos, "datos_migracion.xlsx", row.names = F)

n_distinct(datos$country)


#############################################
### PARA EXPORTAR A ARCGIS ##################
#############################################

datos_60 <- datos %>% filter(year==1960)
write.table(datos_60, "datos_60.txt", col.names = T, row.names = FALSE)
datos_20 <- datos %>% filter(year==2020)
write.table(datos_20, "datos_20.txt", col.names = T, row.names = FALSE)

unique(datos$country)
m1 <- plm((urb)~ mean_temperatura, data = datos, model = "between")

m2 <- fixest::feols(log(urb)~(mean_temperatura) +  
                      log(barrios_precipitacion)
                    + csw0(log(gdp), log(pop) )
                    | year + country
                    , data = datos
                    , panel.id = ~year + country
                    , subset = datos$year<2021
                    #, vcov = NW(1)
                    )
etable(m2)

# urb 60-20
# clima 60-20
# gdp 60-20
# pop 60-20
# dem 60- 20
# serv 94-20
# manu 94-20
# trade 88-20
# slums cada 5 years desde el 90 #eliminar a cuba, belize, chile, costa rica
# ecuador, honduras, haiti, mexico, nicaragua, peru
# city 60-20
# aglo 60-20



library(stargazer)
stargazer(m1, se= diag(sqrt(vcovDC(m1))), type = "text")
