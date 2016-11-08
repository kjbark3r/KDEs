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
elk <- read.csv("collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
elk$Date <- as.Date(elk$Date, "%Y-%m-%d")

##SEASONAL KDES BY SEX

indiv <- subset(elk, AnimalID == 151580)
xy <- data.frame("x"=indiv$Long,"y"=indiv$Lat)
ll <- SpatialPointsDataFrame(xy, indiv, proj4string = latlong)
kde <- kernelUD(ll, h="href", grid = 5000)
raster <- raster(kde)
writeRaster(raster, paste("KDE151580"), format="GTiff", overwrite=TRUE)
