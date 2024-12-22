#REGRESION OLS (todas las variables)
OLS_esferico_log <- lm(formula = log_krig_e ~ M2LOTE + DIST_AGUA  
                + DIST_CLOACAS + DIST_C_SALUD + DIST_ESCUELAS  +
                  DIST_ESPACIOS_V + DIST_BARRIOS + 
                  DIST_ESTACIONES + DIST_RED_ELECTRICA  + 
                  DIST_BASURALES + DIST_RED_VIAL + 
                  DIST_AREA_INUNDABLE + 
                  DIST_PARADAS_BUS, data=esferico_log)

#Resumen OLS
summary(OLS_esferico_log)
#Estadisticos
ols_regress(OLS_esferico_log)
#VIF
ols_vif_tol(OLS_esferico_log)

#Con estas 3 pruebas podemos concluir que se puede extraer del modelo
#las siguientes variables: red electrica y basurales por un p-valor
#mayor que 0.05, por lo tanto no pueden rechazar hipotesis nula por lo tanto
#su incorporacion no es estadisticamente significativa (mejorar)
#por otro lado, el analisis VIF muestra la presencia de variables con multicoesfericoidad. 
#en este caso, cloacas encuentra un VIF mayor a 7.5, lo cual indica alta multicoesfericoidad

#REGRESION OLS (sin red electrica, basurales ni cloacas)

rev_OLS_esferico_log <- lm(formula = log_krig_e ~ M2LOTE + DIST_AGUA  
                            + DIST_C_SALUD + DIST_ESCUELAS  +
                             DIST_ESPACIOS_V + DIST_BARRIOS + 
                             DIST_ESTACIONES +  
                             DIST_RED_VIAL + 
                             DIST_AREA_INUNDABLE + 
                             DIST_PARADAS_BUS, data=esferico_log)

#Resumen OLS (https://rpubs.com/quarcs-lab/tutorial-gwr1)

#par(mfrow=c(2,2))
#plot(rev_OLS_esferico_log)

#plot(1:30)

#Resumen OLS
OLS_rev_summary <- summary(rev_OLS_esferico_log)
OLS_rev_summary
#VIF
ols_vif_tol(rev_OLS_esferico_log)
#AIC
ols_aic(rev_OLS_esferico_log)
#estadisticos
ols_regress(rev_OLS_esferico_log)

#esfericidad
ols_plot_resid_fit(rev_OLS_esferico_log)
#distribucion normal de los residuales
ols_plot_resid_hist(rev_OLS_esferico_log)

#test estadisticos de normalidad

#la prueba de Shapiro requiere de una muestra entre 3 y 5000
#sampling
sample_model = esferico_log[0:5000, c(1:20)]

sample_rev_OLS_esferico_log <- lm(formula = log_krig_e ~ M2LOTE + DIST_AGUA  
                                  + DIST_C_SALUD + DIST_ESCUELAS  +
                                    DIST_ESPACIOS_V + DIST_BARRIOS + 
                                    DIST_ESTACIONES +  
                                    DIST_RED_VIAL + 
                                    DIST_AREA_INUNDABLE + 
                                    DIST_PARADAS_BUS, data=sample_model)

ols_test_normality(sample_rev_OLS_esferico_log)
#la prueba revela que los valores p de los 4 modelos son mas pequeños
#que el p-valor de contraste, por lo cual es posible rechazar la hipotesis
#nula que los valores no estan normalmente distribuidos

#distribucion espacial de residuales #revisar mapas
ols_resid<-as.data.frame(rev_OLS_esferico_log$residuals)

sp_ols_resid <- cbind(esferico_log, ols_resid)

tmap_mode("view")

  tm_shape(sp_ols_resid) +  
  tm_dots(col = "rev_OLS_esferico_log.residuals", title = "OLS_residuals",
          alpha = 0.4,
          style="quantile") +
    tmap_options(check.and.fix = TRUE)  +
  tm_basemap(leaflet::providers$CartoDB.PositronNoLabels) 
  
  tmap_mode("plot")
  
#se encuentra un agrupamiento espacial entre los valores residuales con mayor y menor ajuste

#Prueba de autocorrelacion espacial (I Moran)

#matriz de distancia basada en vecinos mas cercanos (k = 5)
  
centroid_sp_esferico_log <- st_centroid(sp_ols_resid)
knn_esferico_log <- knn2nb(knearneigh(centroid_sp_esferico_log, k = 5)) 

dist <- nbdists(knn_esferico_log, centroid_sp_esferico_log)
# head(dist1)
summary(unlist(dist)) #min 8.12 y max 1827.27 mts con 5 vecinos

nb <- dnearneigh(x = centroid_sp_esferico_log, d1 = 8.12, d2 = 1827.27, longlat = FALSE)
summary(nb)

nb_lw <- nb2listw(nb, style = "S",zero.policy = TRUE)
summary(nb_lw)

i.moran<-lm.morantest(rev_OLS_esferico_log, nb_lw)
i.moran
#p valor esperado < 2.2e-16 para aceptar la hip nula de residuos aleatorios 
#Valor observado de  1.953 > 0.00000000000000022 por lo tanto rechaza H0