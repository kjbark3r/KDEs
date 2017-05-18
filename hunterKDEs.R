###############################################
############ hunter kdes  ##############
############## KJB  Aug 2016  #############
###############################################

## WD

wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\KDEs"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\KDEs"
	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		  cat("Are you SURE you got that file path right?\n")
		  }
	}
rm(wd_workcomp, wd_laptop)

##LOAD PACKAGES
# may not need all of these
  library(sp) #for kernel centroid estimate
  library(adehabitatHR) #for kernel centroid estimate
  library(raster)
  library(rgdal)
  library(gsubfn)
  library(maptools) #for writeSpatialShape
  library(dplyr) #for between()

#DEFINE PROJECTIONS
  latlong = CRS("+init=epsg:4326")
  
##COLLAR DATA
elk <- read.csv("../ElkDatabase/collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
elk$Date <- as.Date(elk$Date, "%Y-%m-%d")

##SEASONAL KDES BY SEX

# AnimalID by DeviceID
unique(elk[elk$DeviceID == 34914, 1])
indiv <- subset(elk, AnimalID == 140380) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)



kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE140380"), format="GTiff", overwrite=TRUE)


##SEASONAL KDES FOR PRESENTATION EXAMPLE


## resident: 140630-15 ##

#winter
indiv <- elk %>%
  subset(AnimalID == 140630) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE140630-15win"), format="GTiff", overwrite=TRUE)
#summer
indiv <- elk %>%
  subset(AnimalID == 140630) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE140630-15sum"), format="GTiff", overwrite=TRUE)



## migrant: 141150-15 ##

#winter
indiv <- elk %>%
  subset(AnimalID == 141150) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE141150-15win"), format="GTiff", overwrite=TRUE)
#summer
indiv <- elk %>%
  subset(AnimalID == 141150) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE141150-15sum"), format="GTiff", overwrite=TRUE)



## intermediate: 141490-15 ##

#winter
indiv <- elk %>%
  subset(AnimalID == 141490) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-02-15"), as.Date("2015-03-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE141490-15win"), format="GTiff", overwrite=TRUE)
#summer
indiv <- elk %>%
  subset(AnimalID == 141490) %>%
  within(Date <- as.Date(Date, format = "%Y-%m-%d")) %>% 
    subset(between(Date, as.Date("2015-07-15"), as.Date("2015-08-31"))) 
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE141490-15sum"), format="GTiff", overwrite=TRUE)
