rm(list = ls())
graphics.off()
clc <- function() cat("\014") 
clc()


library(tidyverse); library(data.table)


#############################################3
########## CARGAR PASES DE DATOS #############
##############################################

### Argentina

Argentina_tas <- fread("tas_timeseries_annual_cru_1901-2021_ARG.csv")
glimpse(Argentina_tas)

Argentina_tas <- fread("tas_timeseries_annual_cru_1901-2021_ARG.csv") %>% 
  select(V1, Argentina) %>% rename(year=V1, temperatura=Argentina) %>% 
  mutate(code="ARG", country="Argentina", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Argentina_tas)


Argentina_pr <- fread("pr_timeseries_annual_cru_1901-2021_ARG.csv")
glimpse(Argentina_pr)

Argentina_pr <- fread("pr_timeseries_annual_cru_1901-2021_ARG.csv") %>% 
  select(V1, Argentina) %>% rename(year=V1, precipitacion=Argentina) %>% 
  mutate(code="ARG", country="Argentina", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Argentina_pr)

Argentina_clima = left_join(Argentina_pr, Argentina_tas
                         , by = c("year", "code", "country"))

glimpse(Argentina_clima)
datos_clima <- Argentina_clima
glimpse(datos_clima)

### Belize


Belize_tas <- fread("tas_timeseries_annual_cru_1901-2021_BLZ.csv")
Belize_tas <- Belize_tas[,-3]
glimpse(Belize_tas)
Belize_tas$Belize

Belize_tas <- Belize_tas %>% 
  select(V1, Belize) %>% rename(year=V1, temperatura=Belize) %>% 
  mutate(code="BLZ", country="Belize", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Belize_tas)


Belize_pr <- fread("pr_timeseries_annual_cru_1901-2021_BLZ.csv")
Belize_pr <- Belize_pr[,-3]
glimpse(Belize_pr)

Belize_pr <- Belize_pr %>% 
  select(V1, Belize) %>% rename(year=V1, precipitacion=Belize) %>% 
  mutate(code="BLZ", country="Belize", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Belize_pr)

Belize_clima = left_join(Belize_pr, Belize_tas
                         , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Belize_clima)
unique(datos_clima$country)

### Boliva

Bolivia_tas <- fread("tas_timeseries_annual_cru_1901-2021_BOL.csv")
glimpse(Bolivia_tas)

Bolivia_tas <- fread("tas_timeseries_annual_cru_1901-2021_BOL.csv") %>% 
  select(V1, Bolivia) %>% rename(year=V1, temperatura=Bolivia) %>% 
  mutate(code="BOL", country="Bolivia", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Bolivia_tas)


Bolivia_pr <- fread("pr_timeseries_annual_cru_1901-2021_BOL.csv")
glimpse(Bolivia_pr)

Bolivia_pr <- fread("pr_timeseries_annual_cru_1901-2021_BOL.csv") %>% 
  select(V1, Bolivia) %>% rename(year=V1, precipitacion=Bolivia) %>% 
  mutate(code="BOL", country="Bolivia", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Bolivia_pr)

Bolivia_clima = left_join(Bolivia_pr, Bolivia_tas
                         , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Bolivia_clima)
unique(datos_clima$country)



### Brazil  

Brazil_tas <- fread("tas_timeseries_annual_cru_1901-2021_BRA.csv")
glimpse(Brazil_tas)

Brazil_tas <- fread("tas_timeseries_annual_cru_1901-2021_BRA.csv") %>% 
  select(V1, Brazil) %>% rename(year=V1, temperatura=Brazil) %>% 
  mutate(code="BRA", country="Brazil", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Brazil_tas)


Brazil_pr <- fread("pr_timeseries_annual_cru_1901-2021_BRA.csv")
glimpse(Brazil_pr)

Brazil_pr <- fread("pr_timeseries_annual_cru_1901-2021_BRA.csv") %>% 
  select(V1, Brazil) %>% rename(year=V1, precipitacion=Brazil) %>% 
  mutate(code="BRA", country="Brazil", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Brazil_pr)

Brazil_clima = left_join(Brazil_pr, Brazil_tas
                          , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Brazil_clima)
unique(datos_clima$country)


### Chile  

Chile_tas <- fread("tas_timeseries_annual_cru_1901-2021_CHL.csv")
glimpse(Chile_tas)

Chile_tas <- fread("tas_timeseries_annual_cru_1901-2021_CHL.csv") %>% 
  select(V1, Chile) %>% rename(year=V1, temperatura=Chile) %>% 
  mutate(code="CHL", country="Chile", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Chile_tas)


Chile_pr <- fread("pr_timeseries_annual_cru_1901-2021_CHL.csv")
glimpse(Chile_pr)

Chile_pr <- fread("pr_timeseries_annual_cru_1901-2021_CHL.csv") %>% 
  select(V1, Chile) %>% rename(year=V1, precipitacion=Chile) %>% 
  mutate(code="CHL", country="Chile", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Chile_pr)

Chile_clima = left_join(Chile_pr, Chile_tas
                         , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Chile_clima)
unique(datos_clima$country)


### Colombia  

Colombia_tas <- fread("tas_timeseries_annual_cru_1901-2021_COL.csv")
glimpse(Colombia_tas)

Colombia_tas <- fread("tas_timeseries_annual_cru_1901-2021_COL.csv") %>% 
  select(V1, Colombia) %>% rename(year=V1, temperatura=Colombia) %>% 
  mutate(code="COL", country="Colombia", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Colombia_tas)


Colombia_pr <- fread("pr_timeseries_annual_cru_1901-2021_COL.csv")
glimpse(Colombia_pr)

Colombia_pr <- fread("pr_timeseries_annual_cru_1901-2021_COL.csv") %>% 
  select(V1, Colombia) %>% rename(year=V1, precipitacion=Colombia) %>% 
  mutate(code="COL", country="Colombia", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Colombia_pr)

Colombia_clima = left_join(Colombia_pr, Colombia_tas
                        , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Colombia_clima)
unique(datos_clima$country)


### CRI  

CRI_tas <- fread("tas_timeseries_annual_cru_1901-2021_CRI.csv")
glimpse(CRI_tas)

CRI_tas <- fread("tas_timeseries_annual_cru_1901-2021_CRI.csv") %>% 
  select(V1, `Costa Rica`) %>% rename(year=V1, temperatura=`Costa Rica`) %>% 
  mutate(code="CRI", country="Costa Rica", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(CRI_tas)


CRI_pr <- fread("pr_timeseries_annual_cru_1901-2021_CRI.csv")
glimpse(CRI_pr)

CRI_pr <- fread("pr_timeseries_annual_cru_1901-2021_CRI.csv") %>% 
  select(V1, `Costa Rica`) %>% rename(year=V1, precipitacion=`Costa Rica`) %>% 
  mutate(code="CRI", country="Costa Rica", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(CRI_pr)

CRI_clima = left_join(CRI_pr, CRI_tas
                           , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, CRI_clima)
unique(datos_clima$country)


### Cuba  

Cuba_tas <- fread("tas_timeseries_annual_cru_1901-2021_CUB.csv")
glimpse(Cuba_tas)

Cuba_tas <- fread("tas_timeseries_annual_cru_1901-2021_CUB.csv") %>% 
  select(V1, Cuba) %>% rename(year=V1, temperatura=Cuba) %>% 
  mutate(code="CUB", country="Cuba", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Cuba_tas)


Cuba_pr <- fread("pr_timeseries_annual_cru_1901-2021_CUB.csv")
glimpse(Cuba_pr)

Cuba_pr <- fread("pr_timeseries_annual_cru_1901-2021_CUB.csv") %>% 
  select(V1, Cuba) %>% rename(year=V1, precipitacion=Cuba) %>% 
  mutate(code="CUB", country="Cuba", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Cuba_pr)

Cuba_clima = left_join(Cuba_pr, Cuba_tas
                           , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Cuba_clima)
unique(datos_clima$country)


### Cuba  

DOM_tas <- fread("tas_timeseries_annual_cru_1901-2021_DOM.csv")
glimpse(DOM_tas)

DOM_tas <- fread("tas_timeseries_annual_cru_1901-2021_DOM.csv") %>% 
  select(V1, `Dominican Republic`) %>% rename(year=V1, temperatura=`Dominican Republic`) %>% 
  mutate(code="DOM", country="Dominican Republic", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(DOM_tas)


DOM_pr <- fread("pr_timeseries_annual_cru_1901-2021_DOM.csv")
glimpse(DOM_pr)

DOM_pr <- fread("pr_timeseries_annual_cru_1901-2021_DOM.csv") %>% 
  select(V1, `Dominican Republic`) %>% rename(year=V1, precipitacion=`Dominican Republic`) %>% 
  mutate(code="DOM", country="Dominican Republic", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(DOM_pr)

DOM_clima = left_join(DOM_pr, DOM_tas
                       , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, DOM_clima)
unique(datos_clima$country)


### Ecuador  

Ecuador_tas <- fread("tas_timeseries_annual_cru_1901-2021_ECU.csv")
glimpse(Ecuador_tas)

Ecuador_tas <- fread("tas_timeseries_annual_cru_1901-2021_ECU.csv") %>% 
  select(V1, Ecuador) %>% rename(year=V1, temperatura=Ecuador) %>% 
  mutate(code="ECU", country="Ecuador", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Ecuador_tas)


Ecuador_pr <- fread("pr_timeseries_annual_cru_1901-2021_ECU.csv")
glimpse(Ecuador_pr)

Ecuador_pr <- fread("pr_timeseries_annual_cru_1901-2021_ECU.csv") %>% 
  select(V1, Ecuador) %>% rename(year=V1, precipitacion=Ecuador) %>% 
  mutate(code="ECU", country="Ecuador", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Ecuador_pr)

Ecuador_clima = left_join(Ecuador_pr, Ecuador_tas
                       , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Ecuador_clima)
unique(datos_clima$country)


### Guatemala  

Guatemala_tas <- fread("tas_timeseries_annual_cru_1901-2021_GTM.csv")
Guatemala_tas <- Guatemala_tas[,-3]
glimpse(Guatemala_tas)

Guatemala_tas <- Guatemala_tas %>% 
  select(V1, Guatemala) %>% rename(year=V1, temperatura=Guatemala) %>% 
  mutate(code="GTM", country="Guatemala", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Guatemala_tas)


Guatemala_pr <- fread("pr_timeseries_annual_cru_1901-2021_GTM.csv")
Guatemala_pr <- Guatemala_pr[,-3]
glimpse(Guatemala_pr)

Guatemala_pr <- Guatemala_pr %>% 
  select(V1, Guatemala) %>% rename(year=V1, precipitacion=Guatemala) %>% 
  mutate(code="GTM", country="Guatemala", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Guatemala_pr)

Guatemala_clima = left_join(Guatemala_pr, Guatemala_tas
                          , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Guatemala_clima)
unique(datos_clima$country)


### Honduras  

Honduras_tas <- fread("tas_timeseries_annual_cru_1901-2021_HND.csv")
glimpse(Honduras_tas)

Honduras_tas <- fread("tas_timeseries_annual_cru_1901-2021_HND.csv") %>% 
  select(V1, Honduras) %>% rename(year=V1, temperatura=Honduras) %>% 
  mutate(code="HND", country="Honduras", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Honduras_tas)


Honduras_pr <- fread("pr_timeseries_annual_cru_1901-2021_HND.csv")
glimpse(Honduras_pr)

Honduras_pr <- fread("pr_timeseries_annual_cru_1901-2021_HND.csv") %>% 
  select(V1, Honduras) %>% rename(year=V1, precipitacion=Honduras) %>% 
  mutate(code="HND", country="Honduras", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Honduras_pr)

Honduras_clima = left_join(Honduras_pr, Honduras_tas
                          , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Honduras_clima)
unique(datos_clima$country)


### Haiti  

Haiti_tas <- fread("tas_timeseries_annual_cru_1901-2021_HTI.csv")
glimpse(Haiti_tas)

Haiti_tas <- fread("tas_timeseries_annual_cru_1901-2021_HTI.csv") %>% 
  select(V1, Haiti) %>% rename(year=V1, temperatura=Haiti) %>% 
  mutate(code="HTI", country="Haiti", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Haiti_tas)


Haiti_pr <- fread("pr_timeseries_annual_cru_1901-2021_HTI.csv")
glimpse(Haiti_pr)

Haiti_pr <- fread("pr_timeseries_annual_cru_1901-2021_HTI.csv") %>% 
  select(V1, Haiti) %>% rename(year=V1, precipitacion=Haiti) %>% 
  mutate(code="HTI", country="Haiti", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Haiti_pr)

Haiti_clima = left_join(Haiti_pr, Haiti_tas
                           , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Haiti_clima)
unique(datos_clima$country)

### Mexico  

Mexico_tas <- fread("tas_timeseries_annual_cru_1901-2021_MEX.csv")

Mexico_tas <- Mexico_tas[,1:2]
glimpse(Mexico_tas)

Mexico_tas <- Mexico_tas %>% 
  select(V1, Mexico) %>% rename(year=V1, temperatura=Mexico) %>% 
  mutate(code="MEX", country="Mexico", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Mexico_tas)


Mexico_pr <- fread("pr_timeseries_annual_cru_1901-2021_MEX.csv")
Mexico_pr <- Mexico_pr[,1:2]
glimpse(Mexico_pr)

Mexico_pr <- Mexico_pr %>% 
  select(V1, Mexico) %>% rename(year=V1, precipitacion=Mexico) %>% 
  mutate(code="MEX", country="Mexico", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Mexico_pr)

Mexico_clima = left_join(Mexico_pr, Mexico_tas
                        , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Mexico_clima)
unique(datos_clima$country)


### Nicaragua  

Nicaragua_tas <- fread("tas_timeseries_annual_cru_1901-2021_NIC.csv")
glimpse(Nicaragua_tas)

Nicaragua_tas <- fread("tas_timeseries_annual_cru_1901-2021_NIC.csv") %>% 
  select(V1, Nicaragua) %>% rename(year=V1, temperatura=Nicaragua) %>% 
  mutate(code="NIC", country="Nicaragua", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Nicaragua_tas)


Nicaragua_pr <- fread("pr_timeseries_annual_cru_1901-2021_NIC.csv")
glimpse(Nicaragua_pr)

Nicaragua_pr <- fread("pr_timeseries_annual_cru_1901-2021_NIC.csv") %>% 
  select(V1, Nicaragua) %>% rename(year=V1, precipitacion=Nicaragua) %>% 
  mutate(code="NIC", country="Nicaragua", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Nicaragua_pr)

Nicaragua_clima = left_join(Nicaragua_pr, Nicaragua_tas
                        , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Nicaragua_clima)
unique(datos_clima$country)

### Panama  

Panama_tas <- fread("tas_timeseries_annual_cru_1901-2021_PAN.csv")
glimpse(Panama_tas)

Panama_tas <- fread("tas_timeseries_annual_cru_1901-2021_PAN.csv") %>% 
  select(V1, Panama) %>% rename(year=V1, temperatura=Panama) %>% 
  mutate(code="PAN", country="Panama", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Panama_tas)


Panama_pr <- fread("pr_timeseries_annual_cru_1901-2021_PAN.csv")
glimpse(Panama_pr)

Panama_pr <- fread("pr_timeseries_annual_cru_1901-2021_PAN.csv") %>% 
  select(V1, Panama) %>% rename(year=V1, precipitacion=Panama) %>% 
  mutate(code="PAN", country="Panama", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Panama_pr)

Panama_clima = left_join(Panama_pr, Panama_tas
                            , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Panama_clima)
unique(datos_clima$country)


### Peru  

Peru_tas <- fread("tas_timeseries_annual_cru_1901-2021_PER.csv")
glimpse(Peru_tas)

Peru_tas <- fread("tas_timeseries_annual_cru_1901-2021_PER.csv") %>% 
  select(V1, Peru) %>% rename(year=V1, temperatura=Peru) %>% 
  mutate(code="PER", country="Peru", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Peru_tas)


Peru_pr <- fread("pr_timeseries_annual_cru_1901-2021_PER.csv")
glimpse(Peru_pr)

Peru_pr <- fread("pr_timeseries_annual_cru_1901-2021_PER.csv") %>% 
  select(V1, Peru) %>% rename(year=V1, precipitacion=Peru) %>% 
  mutate(code="PER", country="Peru", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Peru_pr)

Peru_clima = left_join(Peru_pr, Peru_tas
                         , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Peru_clima)
unique(datos_clima$country)

### Paraguay  

Paraguay_tas <- fread("tas_timeseries_annual_cru_1901-2021_PRY.csv")
glimpse(Paraguay_tas)

Paraguay_tas <- fread("tas_timeseries_annual_cru_1901-2021_PRY.csv") %>% 
  select(V1, Paraguay) %>% rename(year=V1, temperatura=Paraguay) %>% 
  mutate(code="PRY", country="Paraguay", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Paraguay_tas)


Paraguay_pr <- fread("pr_timeseries_annual_cru_1901-2021_PRY.csv")
glimpse(Paraguay_pr)

Paraguay_pr <- fread("pr_timeseries_annual_cru_1901-2021_PRY.csv") %>% 
  select(V1, Paraguay) %>% rename(year=V1, precipitacion=Paraguay) %>% 
  mutate(code="PRY", country="Paraguay", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Paraguay_pr)

Paraguay_clima = left_join(Paraguay_pr, Paraguay_tas
                       , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Paraguay_clima)
unique(datos_clima$country)


### SLV  

SLV_tas <- fread("tas_timeseries_annual_cru_1901-2021_SLV.csv")
glimpse(SLV_tas)

SLV_tas <- fread("tas_timeseries_annual_cru_1901-2021_SLV.csv") %>% 
  select(V1, `El Salvador`) %>% rename(year=V1, temperatura=`El Salvador`) %>% 
  mutate(code="SLV", country="El Salvador", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(SLV_tas)


SLV_pr <- fread("pr_timeseries_annual_cru_1901-2021_SLV.csv")
glimpse(SLV_pr)

SLV_pr <- fread("pr_timeseries_annual_cru_1901-2021_SLV.csv") %>% 
  select(V1, `El Salvador`) %>% rename(year=V1, precipitacion=`El Salvador`) %>% 
  mutate(code="SLV", country="El Salvador", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(SLV_pr)

SLV_clima = left_join(SLV_pr, SLV_tas
                           , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, SLV_clima)
unique(datos_clima$country)


### Uruguay  

Uruguay_tas <- fread("tas_timeseries_annual_cru_1901-2021_URY.csv")
glimpse(Uruguay_tas)

Uruguay_tas <- fread("tas_timeseries_annual_cru_1901-2021_URY.csv") %>% 
  select(V1, Uruguay) %>% rename(year=V1, temperatura=Uruguay) %>% 
  mutate(code="URY", country="Uruguay", m_temperatura = mean(temperatura)
         , sd_temperatura=sd(temperatura)
         , max_temperatura = max(temperatura)
         , min_temperatura= min(temperatura)) %>% 
  mutate(anoma_temperatura=(temperatura-m_temperatura)/sd_temperatura
         ,mean_temperatura=(temperatura-m_temperatura)/(max_temperatura-min_temperatura)
         ,barrios_temperatura= (temperatura/m_temperatura))
glimpse(Uruguay_tas)


Uruguay_pr <- fread("pr_timeseries_annual_cru_1901-2021_URY.csv")
glimpse(Uruguay_pr)

Uruguay_pr <- fread("pr_timeseries_annual_cru_1901-2021_URY.csv") %>% 
  select(V1, Uruguay) %>% rename(year=V1, precipitacion=Uruguay) %>% 
  mutate(code="URY", country="Uruguay", m_precipitacion = mean(precipitacion)
         , sd_precipitacion=sd(precipitacion)
         , max_precipitacion = max(precipitacion)
         , min_precipitacion= min(precipitacion)) %>% 
  mutate(anoma_precipitacion=(precipitacion-m_precipitacion)/sd_precipitacion
         ,mean_precipitacion=(precipitacion-m_precipitacion)/(max_precipitacion-min_precipitacion)
         ,barrios_precipitacion= (precipitacion/m_precipitacion))
glimpse(Uruguay_pr)

Uruguay_clima = left_join(Uruguay_pr, Uruguay_tas
                           , by = c("year", "code", "country"))

datos_clima <- rbind(datos_clima, Uruguay_clima)
unique(datos_clima$country)


################################################
######### exportar datos #######################
################################################


write.csv(datos_clima, "datos_clima.csv")



