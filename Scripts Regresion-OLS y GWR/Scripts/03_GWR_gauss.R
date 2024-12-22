#https://rpubs.com/tskam/IS415-Hands-on_Ex09
##GWR con bandwith fixed
sp_gaussian_log <- as_Spatial(gaussian_log)

bw.fixed <- bw.gwr(formula = log_kri_g ~ M2LOTE + DIST_AGUA  
                   + DIST_C_SALUD + DIST_ESCUELAS  +
                     DIST_ESPACIOS_V + DIST_BARRIOS + 
                     DIST_ESTACIONES +  
                     DIST_RED_VIAL + 
                     DIST_AREA_INUNDABLE + 
                     DIST_PARADAS_BUS,  
                   data=sp_gaussian_log, adaptive=FALSE)

gwr.fixed.gaussian <- gwr.basic(formula = log_kri_g ~ M2LOTE + DIST_AGUA  
                                + DIST_C_SALUD + DIST_ESCUELAS  +
                                  DIST_ESPACIOS_V + DIST_BARRIOS + 
                                  DIST_ESTACIONES +  
                                  DIST_RED_VIAL + 
                                  DIST_AREA_INUNDABLE + 
                                  DIST_PARADAS_BUS,  
                                data=sp_gaussian_log, bw=bw.fixed, 
                       kernel = "gaussian", longlat = FALSE)
gwr.fixed.gaussian

gwr.fixed.exponential <- gwr.basic(formula = log_kri_g ~ M2LOTE + DIST_AGUA  
                                   + DIST_C_SALUD + DIST_ESCUELAS  +
                                     DIST_ESPACIOS_V + DIST_BARRIOS + 
                                     DIST_ESTACIONES +  
                                     DIST_RED_VIAL + 
                                     DIST_AREA_INUNDABLE + 
                                     DIST_PARADAS_BUS,  
                                   data=sp_gaussian_log, bw=bw.fixed, 
                                   kernel = "exponential", longlat = FALSE)

gwr.fixed.exponential

#GWR fixed: 0.6017884 kernel exponencial - 0.6017379 gaussiano

##GWR con banda  adaptive

bw.adaptive <- bw.gwr(formula = log_kri_g ~ M2LOTE + DIST_AGUA  
                      + DIST_C_SALUD + DIST_ESCUELAS  +
                        DIST_ESPACIOS_V + DIST_BARRIOS + 
                        DIST_ESTACIONES +  
                        DIST_RED_VIAL + 
                        DIST_AREA_INUNDABLE + 
                        DIST_PARADAS_BUS, 
                   data=sp_gaussian_log, adaptive=TRUE)
rm(gwr.adaptive.exponential)
gwr.adaptive.gaussian <- gwr.basic(formula = log_kri_g ~ M2LOTE + DIST_AGUA  
                                   + DIST_C_SALUD + DIST_ESCUELAS  +
                                     DIST_ESPACIOS_V + DIST_BARRIOS + 
                                     DIST_ESTACIONES +  
                                     DIST_RED_VIAL + 
                                     DIST_AREA_INUNDABLE + 
                                     DIST_PARADAS_BUS, data=sp_gaussian_log, 
                         bw=bw.adaptive, 
                       kernel = 'gaussian', adaptive=TRUE, longlat = FALSE)

gwr.adaptive.gaussian

gwr.adaptive.exponential <- gwr.basic(formula =  log_kri_g ~ M2LOTE + DIST_AGUA  
                                      + DIST_C_SALUD + DIST_ESCUELAS  +
                                        DIST_ESPACIOS_V + DIST_BARRIOS + 
                                        DIST_ESTACIONES +  
                                        DIST_RED_VIAL + 
                                        DIST_AREA_INUNDABLE + 
                                        DIST_PARADAS_BUS, data=sp_gaussian_log, bw=bw.adaptive, 
                                   kernel = "exponential", adaptive=TRUE, longlat = FALSE)
gwr.adaptive.exponential

#GWR adaptive: 0.7556332 gaussian - 0.7196237 exponential

#mapeo R2 local
sp.gwr.fixed.gaussian<- st_as_sf(gwr.fixed.gaussian$SDF)
sp.gwr.fixed.exponential<- st_as_sf(gwr.fixed.exponential$SDF)
sp.gwr.adaptive.gaussian<- st_as_sf(gwr.adaptive.gaussian$SDF)
sp.gwr.adaptive.exponential<- st_as_sf(gwr.adaptive.exponential$SDF)



sp.gwr.fixed.gaussian.r2 <- sp.gwr.fixed.gaussian %>% select(Local_R2,geometry)
sp.gwr.fixed.exponential.r2 <- sp.gwr.fixed.exponential %>% select(Local_R2,geometry)
sp.gwr.adaptive.gaussian.r2 <- sp.gwr.adaptive.gaussian %>% select(Local_R2,geometry)
sp.gwr.adaptive.exponential.r2 <- sp.gwr.adaptive.exponential %>% select(Local_R2,geometry)


#exportar shapefile
st_write(sp.gwr.fixed.gaussian.r2, "C:/Users/leand/Desktop/Pilar_ECTAAE/R_OLS_GWR/Output/gaussian_log/R2_local/sp_gwr_fixed_gaussian_r2.shp")
st_write(sp.gwr.fixed.gaussian.r2, "C:/Users/leand/Desktop/Pilar_ECTAAE/R_OLS_GWR/Output/gaussian_log/R2_local/sp.gwr.fixed.exponential.r2.shp")
st_write(sp.gwr.adaptive.gaussian.r2, "C:/Users/leand/Desktop/Pilar_ECTAAE/R_OLS_GWR/Output/gaussian_log/R2_local/sp.gwr.adaptive.gaussian.r2.shp")
st_write(sp.gwr.adaptive.exponential.r2, "C:/Users/leand/Desktop/Pilar_ECTAAE/R_OLS_GWR/Output/gaussian_log/R2_local/sp.gwr.adaptive.exponential.r2.shp")

map.sp.gwr.fixed.gaussian <- tm_shape(sp.gwr.fixed.gaussian) + 
  tm_dots(col = "Local_R2",
          size = 0.20,
          style="quantile")+
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.title.size = 0.8,
            legend.text.size =0.8,
            title.size = 1,
            legend.width = 1)    

map.sp.gwr.fixed.gaussian

map.sp.gwr.fixed.exponential <- tm_shape(sp.gwr.fixed.exponential) + 
  tm_bubbles(col = "Local_R2",
          size = 0.15,
          style="quantile")+
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.title.size = 1,
            legend.text.size =1)    

map.sp.gwr.fixed.exponential

map.sp.gwr.adaptive.gaussian <- tm_shape(sp.gwr.adaptive.gaussian) + 
                                tm_dots(col = "Local_R2",
                                alpha = 0.6,
                                style="quantile")+
                                tm_layout(frame = FALSE,
                                          legend.outside = TRUE,
                                          legend.title.size = 1.5,
                                          legend.text.size =1.5)  

map.sp.gwr.adaptive.gaussian

map.sp.gwr.adaptive.exponential <- tm_shape(sp.gwr.adaptive.exponential) + 
  tm_dots(col = "Local_R2",
          style="quantile",
          size = 0.12) +
  tm_layout(frame = FALSE,
            legend.outside = TRUE,
            legend.title.size = 1.5,
            legend.text.size =1.5)

map.sp.gwr.adaptive.exponential

library(grid)
library(gridExtra)
#Mapeo de R2

grid.newpage()

pushViewport(viewport(layout=grid.layout(2,2)))

print(map.sp.gwr.adaptive.gaussian, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map.sp.gwr.fixed.exponential, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map.sp.gwr.adaptive.gaussian, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map.sp.gwr.adaptive.exponential, vp=viewport(layout.pos.col = 2, layout.pos.row =2))

#ver mapas




