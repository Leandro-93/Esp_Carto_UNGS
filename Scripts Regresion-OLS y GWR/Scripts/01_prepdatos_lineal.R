setwd("C:/Users/leand/Desktop/Pilar_ECTAAE/R_OLS_GWR")

packages = c('corrplot', 'GGally', 'olsrr', 'sf', 'spdep', 'GWmodel', 'tmap', 'tidyverse', 'spgwr', 'ggpubr', 'dplyr')

lapply(packages, require, character.only = TRUE)

lineal_log = st_read(dsn = "01-prep_datos/lineal_log", layer = "lotes_krig_lineal_distancias")

##RENAME
lineal_log = lineal_log %>% rename("COD_PARCELARIO"="CCA","ZONIFICACION"="designacio", "M2LOTE"="area", "DIST_AGUA"="agua", 
                                   "DIST_CLOACAS"="cloacas", "DIST_C_SALUD"="centros_sa", "DIST_ESCUELAS"="escuelas", 
                                   "DIST_ESPACIOS_V"="espacios_v","DIST_BARRIOS"="barrios_po", "DIST_ESTACIONES"="estaciones", 
                                   "DIST_RED_ELECTRICA"="red_elect","DIST_BASURALES"= "basurales", 
                                   "DIST_RED_VIAL" ="red_vial", "DIST_AREA_INUNDABLE"="inundacion","DIST_PARADAS_BUS"="paradas_co")


##EDA VDEP
##Histo VDEP USD x M2
ggplot(data=lineal_log, aes(x=`USDxM2`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+ 
  labs(y = "Lotes", x = "USDxM2")

##Histogramas

M2LOTE <- ggplot(data=lineal_log, aes(x= `M2LOTE`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_RED_AGUA <- ggplot(data=lineal_log, aes(x= `DIST_AGUA`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_RED_CLOACAS <- ggplot(data=lineal_log, aes(x= `DIST_CLOACAS`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_CENTROS_SALUD <- ggplot(data=lineal_log, aes(x= `DIST_C_SALUD`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_ESCUELAS <- ggplot(data=lineal_log, aes(x= `DIST_ESCUELAS`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_ESPACIOS_VERDES <- ggplot(data=lineal_log, aes(x= `DIST_ESPACIOS_V`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_BARRIOS_POP <- ggplot(data=lineal_log, aes(x= `DIST_BARRIOS`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_ESTACIONES <- ggplot(data=lineal_log, aes(x= `DIST_ESTACIONES`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_RED_ELECTRICA <- ggplot(data=lineal_log, aes(x= `DIST_RED_ELECTRICA`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_BASURALES <- ggplot(data=lineal_log, aes(x= `DIST_BASURALES`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_RED_VIAL <- ggplot(data=lineal_log, aes(x= `DIST_RED_VIAL`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_INUNDACION <- ggplot(data=lineal_log, aes(x= `DIST_AREA_INUNDABLE`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())
PROX_PARADAS_COL<- ggplot(data=lineal_log, aes(x= `DIST_PARADAS_BUS`)) +
  geom_histogram(bins=20, color="black", fill="cornflowerblue")+
  theme(axis.title.y = element_blank())

ggarrange(M2LOTE, PROX_RED_AGUA, PROX_RED_CLOACAS, PROX_CENTROS_SALUD, 
          PROX_ESCUELAS, PROX_ESPACIOS_VERDES, PROX_BARRIOS_POP, 
          PROX_ESTACIONES, PROX_RED_ELECTRICA, PROX_BASURALES, PROX_RED_VIAL, 
          PROX_INUNDACION,  PROX_PARADAS_COL ,ncol = 4, nrow = 4)
#Podemos ver que la distribucion de gran parte de las variables es normal,
#con un sesgo hacia la derecha en M2_lote, PROX_ESCUELAS, CENTROS_SALUD, 
#RED_ELECT y RED_VIAL. Esto quiere decir que estas varaibles se encuentran
#mas cercanas a los lotes o ,en el caso de los M2, se concentran en tamaños
#menores a 10000 m2

str(numeric_lineal_log)

#DISPERSION
numeric_lineal_log <- lineal_log[, sapply(lineal_log, is.numeric)] %>% st_drop_geometry() %>% select(-c("USDxM2", "ID", "krig_linea", "log_krig_l")) 

numeric_lineal_log = numeric_lineal_log %>% rename("M2L"="M2LOTE", "D_AG"="DIST_AGUA", 
                                                       "D_CL"="DIST_CLOACAS", "D_C_S"="DIST_C_SALUD", "D_ESC"="DIST_ESCUELAS", 
                                                       "D_E_V"="DIST_ESPACIOS_V","D_BP"="DIST_BARRIOS", "D_EST"="DIST_ESTACIONES", 
                                                       "D_R_E"="DIST_RED_ELECTRICA","D_BA"= "DIST_BASURALES", 
                                                       "D_R_V" ="DIST_RED_VIAL", "D_IN"="DIST_AREA_INUNDABLE","D_P_B"="DIST_PARADAS_BUS")

scatter_1 <-numeric_lineal_log %>% ggpairs()
print(scatter_1)

#CORRELACION

cor_values <- cor(numeric_lineal_log)
round(cor_values, digits = 2)
corrplot(cor_values, type = 'lower', tl.cex = 0.8, tl.col = 'black',tl.srt =90)
