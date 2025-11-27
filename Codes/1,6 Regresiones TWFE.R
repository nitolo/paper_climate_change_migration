rm(list = ls())
graphics.off()
clc <- function() cat("\014") 
clc()


library(tidyverse)
library(data.table)
library(plm)
library(lubridate)
library(fixest)
library(stargazer)
library(readxl)
library(openxlsx)
####################################
#### CARGAR BASE DE DATOS ##########
####################################

migracion <- read_xlsx("datos_migracion.xlsx") %>% 
  filter(year <=2019 & country != "Cuba") 
n_unik(migracion$country)
glimpse(migracion)

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


migracion_5 <- migracion %>% 
  group_by(quin, country) %>% 
  summarise(
     manu = mean(manu, na.rm=T)
    ,serv = mean(serv, na.rm=T)
    ,gdp  = mean(gdp, na.rm=T)
    ,urb  = mean(urb, na.rm=T)
    ,pop  = mean(pop, na.rm=T)
    ,pop_work = mean(pop_work, na.rm=T)
    ,trade = mean(trade, na.rm=T)
    ,city = mean(city, na.rm=T)
    ,aglo = mean(aglo, na.rm=T)
    ,dem = mean(dem, na.rm=T)
    ,anoma_precipitacion = mean(anoma_precipitacion, na.rm=T)
    ,mean_precipitacion = mean(mean_precipitacion, na.rm=T)
    ,barrios_precipitacion = mean(barrios_precipitacion, na.rm=T)
    ,anoma_temperatura = mean(anoma_temperatura, na.rm=T)
    ,mean_temperatura = mean(mean_temperatura, na.rm=T)
    ,barrios_temperatura = mean(barrios_temperatura, na.rm=T)
  ) 
glimpse(migracion_5)

migracion_5.1 <- plm::make.pbalanced(migracion_5
                                     , balance.type = "shared.times")

migracion2 <- pdata.frame(migracion, index = c("country", "year"))
plm::is.pconsecutive(migracion2)
plm::is.pbalanced(migracion2)
glimpse(migracion)

####################################
#### EFECTOS FIJOS #################
####################################

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


m_mean <- feols(c(log(urb), log(city), log(aglo))~
              (mean_temperatura)
            + (mean_precipitacion)
            + csw0(
              log(gdp)# gdp 60-20
              , log(pop_work) # pop 60-20
              , dem # dem 60- 20
              , trade # trade 88-20
              , manu # manu 94-20
              , serv # serv 94-20
              ) 
            | quin + country
            , data = migracion_5
            , panel.id = ~ country + quin
            , cluster = migracion_5$country
            )

etable(m_mean
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

fe_mean <- etable(m_mean
                 , digits = 3 #digitos que van a aparecer
                 , se.below = T # errores estandar abajo de los coefs
                 , tex = F # que sea un data frame y no cod latex
                 , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                 , fitstat = c("n","r2", "aic"))

write.xlsx(fe_mean # nombre del data frame
           ,"reg_panel_fe_mean.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)

m_mean_ols <- feols(c(log(urb), log(city), log(aglo))~
                         (mean_temperatura)
                       + (mean_precipitacion)
                       + log(gdp)# gdp 60-20
                       + log(pop_work) # pop 60-20
                       + dem # dem 60- 20
                       + trade # trade 88-20
                       + manu # manu 94-20
                       + serv # serv 94-20
                       , data = migracion_5
                       , panel.id = ~ country + quin
                       , cluster = migracion_5$country
)

etable(m_mean_ols
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

ols_mean <- etable(m_mean_ols
                      , digits = 3 #digitos que van a aparecer
                      , se.below = T # errores estandar abajo de los coefs
                      , tex = F # que sea un data frame y no cod latex
                      , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                      , fitstat = c("n","r2", "aic"))

write.xlsx(ols_mean # nombre del data frame
           ,"reg_panel_ols_mean.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)




m_anoma <- feols(c(log(urb), log(city), log(aglo))~
                  (anoma_temperatura)
                + (anoma_precipitacion)
                + csw0(
                  log(gdp)# gdp 60-20
                  , log(pop_work) # pop 60-20
                  , dem # dem 60- 20
                  , trade # trade 88-20
                  , manu # manu 94-20
                  , serv # serv 94-20
                ) 
                | quin + country
                , data = migracion_5
                , panel.id = ~ country + quin
                , cluster = migracion_5$country
)

etable(m_anoma
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

fe_anoma <- etable(m_anoma
                  , digits = 3 #digitos que van a aparecer
                  , se.below = T # errores estandar abajo de los coefs
                  , tex = F # que sea un data frame y no cod latex
                  , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                  , fitstat = c("n","r2", "aic"))

write.xlsx(fe_anoma # nombre del data frame
           ,"reg_panel_fe_anoma.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)

m_anoma_ols <- feols(c(log(urb), log(city), log(aglo))~
                         (anoma_temperatura)
                       + (anoma_precipitacion)
                       + log(gdp)# gdp 60-20
                       + log(pop_work) # pop 60-20
                       + dem # dem 60- 20
                       + trade # trade 88-20
                       + manu # manu 94-20
                       + serv # serv 94-20
                       , data = migracion_5
                       , panel.id = ~ country + quin
                       , cluster = migracion_5$country
)

etable(m_anoma_ols
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

ols_anoma <- etable(m_anoma_ols
                      , digits = 3 #digitos que van a aparecer
                      , se.below = T # errores estandar abajo de los coefs
                      , tex = F # que sea un data frame y no cod latex
                      , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                      , fitstat = c("n","r2", "aic"))

write.xlsx(ols_anoma # nombre del data frame
           ,"reg_panel_ols_anoma.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)



m_barrios <- feols(c(log(urb), log(city), log(aglo))~
                   (barrios_temperatura)
                 + (barrios_precipitacion)
                 + csw0(
                   log(gdp)# gdp 60-20
                   , log(pop_work) # pop 60-20
                   , dem # dem 60- 20
                   , trade # trade 88-20
                   , manu # manu 94-20
                   , serv # serv 94-20
                 ) 
                 | quin + country
                 , data = migracion_5
                 , panel.id = ~ country + quin
                 , cluster = migracion_5$country
)

etable(m_barrios
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

fe_barrios <- etable(m_barrios
                   , digits = 3 #digitos que van a aparecer
                   , se.below = T # errores estandar abajo de los coefs
                   , tex = F # que sea un data frame y no cod latex
                   , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                   , fitstat = c("n","r2", "aic"))

write.xlsx(fe_barrios # nombre del data frame
           ,"reg_panel_fe_barrios.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)


m_barrios_ols <- feols(c(log(urb), log(city), log(aglo))~
                         (barrios_temperatura)
                       + (barrios_precipitacion)
                       + log(gdp)# gdp 60-20
                       + log(pop_work) # pop 60-20
                       + dem # dem 60- 20
                       + trade # trade 88-20
                       + manu # manu 94-20
                       + serv # serv 94-20
                       , data = migracion_5
                       , panel.id = ~ country + quin
                       , cluster = migracion_5$country
)

etable(m_barrios_ols
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

ols_barrios <- etable(m_barrios_ols
                     , digits = 3 #digitos que van a aparecer
                     , se.below = T # errores estandar abajo de los coefs
                     , tex = F # que sea un data frame y no cod latex
                     , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
                     , fitstat = c("n","r2", "aic"))

write.xlsx(ols_barrios # nombre del data frame
           ,"reg_panel_ols_barrios.xlsx" #nombre que le quiero poner
           , rowNames=T # basicamente, para que no me quite la primera fila
           
)



















ols <- lm(log(urb)~(barrios_temperatura)
                       + (barrios_precipitacion)
                       + log(gdp)# gdp 60-20
                       + log(pop_work) # pop 60-20
                       + dem # dem 60- 20
                       + trade # trade 88-20
                       + manu # manu 94-20
                       + serv # serv 94-20
                       , data = migracion_5
)



summary(ols)


m1.plm <- plm(log(urb)~
                (barrios_temperatura)
              + (barrios_precipitacion)
              + log(gdp)# gdp 60-20
              + log(pop_work) # pop 60-20
              + dem # dem 60- 20
              + trade # trade 88-20
              + manu # manu 94-20
              + serv # serv 94-20
              , data = migracion_5
              , index = c("country", "quin")
              , model = "pooling"
              , effect="individual"
)
summary(m1.plm)


mprueba <- feols(log(urb)~
                   (barrios_temperatura)
                 + (barrios_precipitacion)
                 + log(gdp)# gdp 60-20
                 + log(pop_work) # pop 60-20
                 + dem # dem 60- 20
                 + trade # trade 88-20
                 + manu # manu 94-20
                 + serv # serv 94-20
                 , data = migracion_5
                 , panel.id = ~ country + quin
                 #, cluster = migracion_5$country
            
)

etable(mprueba
       , digits = 3 #digitos que van a aparecer
       , se.below = T # errores estandar abajo de los coefs
       , tex = F # que sea un data frame y no cod latex
       , signif.code = c("***"=0.01, "**"=0.05, "*"=0.10)
       , fitstat = c("n","r2", "aic"))

