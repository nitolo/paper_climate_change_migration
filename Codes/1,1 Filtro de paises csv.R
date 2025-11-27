
library(tidyverse); library(data.table)

############################################
#### ESTABLECER ESCRITORIO #################
############################################

#setwd("C:/Users/black/Downloads")

#############################################
##### CARGAR BASES DE DATOS #################
#############################################

#### Urbanizacion

urbanizacion <- fread("Poblacion urbana.csv", data.table = F)
glimpse(urbanizacion)
unique(urbanizacion$Entity)

urb <- urbanizacion %>% filter( Entity=="Latin America and Caribbean"
                                        |Entity=="Belize"
                                        |Entity=="Costa Rica"
                                        |Entity=="El Salvador"
                                        |Entity=="Guatemala"
                                        |Entity=="Honduras"
                                        |Entity=="Mexico"
                                        |Entity=="Nicaragua"
                                        |Entity=="Panama"
                                        |Entity=="Argentina"
                                        |Entity=="Bolivia"
                                        |Entity=="Brazil"
                                        |Entity=="Chile"
                                        |Entity=="Colombia"
                                        |Entity=="Ecuador"
                                        |Entity=="Paraguay"
                                        |Entity=="Peru"
                                        |Entity=="Uruguay"
                                        |Entity=="Cuba"
                                        |Entity=="Dominican Republic"
                                        |Entity=="Haiti"
                                        )
unique(urb$Entity)

write.csv(urb, "urbanizacion.csv")
#### Poblacion en la ciudad mas grande por pais

ciudad <- fread("Poblacion en la ciudad mas grande por pais.csv", data.table = F)
glimpse(ciudad)
unique(ciudad$Entity)

ciu <- ciudad %>% filter( Entity=="Latin America and Caribbean"
                                |Entity=="Belize"
                                |Entity=="Costa Rica"
                                |Entity=="El Salvador"
                                |Entity=="Guatemala"
                                |Entity=="Honduras"
                                |Entity=="Mexico"
                                |Entity=="Nicaragua"
                                |Entity=="Panama"
                                |Entity=="Argentina"
                                |Entity=="Bolivia"
                                |Entity=="Brazil"
                                |Entity=="Chile"
                                |Entity=="Colombia"
                                |Entity=="Ecuador"
                                |Entity=="Paraguay"
                                |Entity=="Peru"
                                |Entity=="Uruguay"
                                |Entity=="Cuba"
                                |Entity=="Dominican Republic"
                                |Entity=="Haiti"
)
unique(ciu$Entity)
write.csv(ciu, "ciudad.csv")

#### Comercio como porcentaje del PIB

comercio <- fread("Comercio como porcentaje del PIB.csv", data.table = F)
glimpse(comercio)
unique(comercio$Entity)

com <- comercio %>% filter( Entity=="Latin America and Caribbean"
                                |Entity=="Belize"
                                |Entity=="Costa Rica"
                                |Entity=="El Salvador"
                                |Entity=="Guatemala"
                                |Entity=="Honduras"
                                |Entity=="Mexico"
                                |Entity=="Nicaragua"
                                |Entity=="Panama"
                                |Entity=="Argentina"
                                |Entity=="Bolivia"
                                |Entity=="Brazil"
                                |Entity=="Chile"
                                |Entity=="Colombia"
                                |Entity=="Ecuador"
                                |Entity=="Paraguay"
                                |Entity=="Peru"
                                |Entity=="Uruguay"
                                |Entity=="Cuba"
                                |Entity=="Dominican Republic"
                                |Entity=="Haiti"
)
unique(com$Entity)

write.csv(com, "comercio.csv")

#### Poblacion por pais

poblacion <- fread("Poblacion por pais.csv", data.table = F)
glimpse(poblacion)
unique(poblacion$`Country name`)

pob <- poblacion %>% filter( `Country name`=="Latin America and Caribbean"
                            |`Country name`=="Belize"
                            |`Country name`=="Costa Rica"
                            |`Country name`=="El Salvador"
                            |`Country name`=="Guatemala"
                            |`Country name`=="Honduras"
                            |`Country name`=="Mexico"
                            |`Country name`=="Nicaragua"
                            |`Country name`=="Panama"
                            |`Country name`=="Argentina"
                            |`Country name`=="Bolivia"
                            |`Country name`=="Brazil"
                            |`Country name`=="Chile"
                            |`Country name`=="Colombia"
                            |`Country name`=="Ecuador"
                            |`Country name`=="Paraguay"
                            |`Country name`=="Peru"
                            |`Country name`=="Uruguay"
                            |`Country name`=="Cuba"
                            |`Country name`=="Dominican Republic"
                            |`Country name`=="Haiti"
)
unique(pob$`Country name`)
write.csv(pob, "poblacion.csv")

#### Porcentaje de aglomeracion urbana

aglomeracion <- fread("Porcentaje de aglomeracion urbana.csv", data.table = F)
glimpse(aglomeracion)
unique(aglomeracion$Entity)

aglo <- aglomeracion %>% filter( Entity=="Latin America and Caribbean"
                            |Entity=="Belize"
                            |Entity=="Costa Rica"
                            |Entity=="El Salvador"
                            |Entity=="Guatemala"
                            |Entity=="Honduras"
                            |Entity=="Mexico"
                            |Entity=="Nicaragua"
                            |Entity=="Panama"
                            |Entity=="Argentina"
                            |Entity=="Bolivia"
                            |Entity=="Brazil"
                            |Entity=="Chile"
                            |Entity=="Colombia"
                            |Entity=="Ecuador"
                            |Entity=="Paraguay"
                            |Entity=="Peru"
                            |Entity=="Uruguay"
                            |Entity=="Cuba"
                            |Entity=="Dominican Republic"
                            |Entity=="Haiti"
)
unique(aglo$Entity)
write.csv(aglo, "aglomeracion.csv")
#### Porcentaje de aglomeracion urbana

barrios <- fread("Porcentaje de personas en barrios marginales.csv", data.table = F)
glimpse(barrios)
unique(barrios$Entity)

bar <- barrios %>% filter( Entity=="Latin America and Caribbean"
                                 |Entity=="Belize"
                                 |Entity=="Costa Rica"
                                 |Entity=="El Salvador"
                                 |Entity=="Guatemala"
                                 |Entity=="Honduras"
                                 |Entity=="Mexico"
                                 |Entity=="Nicaragua"
                                 |Entity=="Panama"
                                 |Entity=="Argentina"
                                 |Entity=="Bolivia"
                                 |Entity=="Brazil"
                                 |Entity=="Chile"
                                 |Entity=="Colombia"
                                 |Entity=="Ecuador"
                                 |Entity=="Paraguay"
                                 |Entity=="Peru"
                                 |Entity=="Uruguay"
                                 |Entity=="Cuba"
                                 |Entity=="Dominican Republic"
                                 |Entity=="Haiti"
)
unique(bar$Entity)
write.csv(bar, "barrios_marginados.csv")
#### Porcentaje de aglomeracion urbana

pib <- fread("GDP per capita en USD constante.csv", data.table = F)
glimpse(pib)
unique(pib$Entity)

gdp <- pib %>% filter( Entity=="Latin America and Caribbean"
                           |Entity=="Belize"
                           |Entity=="Costa Rica"
                           |Entity=="El Salvador"
                           |Entity=="Guatemala"
                           |Entity=="Honduras"
                           |Entity=="Mexico"
                           |Entity=="Nicaragua"
                           |Entity=="Panama"
                           |Entity=="Argentina"
                           |Entity=="Bolivia"
                           |Entity=="Brazil"
                           |Entity=="Chile"
                           |Entity=="Colombia"
                           |Entity=="Ecuador"
                           |Entity=="Paraguay"
                           |Entity=="Peru"
                           |Entity=="Uruguay"
                           |Entity=="Cuba"
                           |Entity=="Dominican Republic"
                           |Entity=="Haiti"
)
unique(gdp$Entity)
write.csv(gdp, "gdp.csv")

#### Grado de democracia por pais

democracia <- fread("Grado de democracia por pais.csv", data.table = F)
glimpse(democracia)
unique(democracia$Entity)

dem <- democracia %>% filter( Entity=="Latin America and Caribbean"
                       |Entity=="Belize"
                       |Entity=="Costa Rica"
                       |Entity=="El Salvador"
                       |Entity=="Guatemala"
                       |Entity=="Honduras"
                       |Entity=="Mexico"
                       |Entity=="Nicaragua"
                       |Entity=="Panama"
                       |Entity=="Argentina"
                       |Entity=="Bolivia"
                       |Entity=="Brazil"
                       |Entity=="Chile"
                       |Entity=="Colombia"
                       |Entity=="Ecuador"
                       |Entity=="Paraguay"
                       |Entity=="Peru"
                       |Entity=="Uruguay"
                       |Entity=="Cuba"
                       |Entity=="Dominican Republic"
                       |Entity=="Haiti"
)
unique(dem$Entity)
write.csv(dem, "democracia.csv")


