###-#######################################-###
### seasonal kdes for mt dean stone project ###
###-########## KJB  Jul/Aug 2016  #########-###
###-##########  updated Jul 2017  #########-###
###-#######################################-###


#### SETUP ####

## WD ##
  wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\KDEtest"
  wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\KDEtest"
  wd_worklaptop <- "C:\\Users\\kristin\\Documents\\KDEs"
  if (file.exists(wd_workcomp)) {setwd(wd_workcomp)
  } else {
    if(file.exists(wd_laptop)) {setwd(wd_laptop)
    } else {
      setwd(wd_worklaptop)
    }
  }
  rm(wd_workcomp, wd_laptop, wd_worklaptop)
  

## PACKAGES ##
# may not need all of these
  library(sp) #for kernel centroid estimate
  library(adehabitatHR) #for kernel centroid estimate
  library(raster)
  library(rgdal)
  library(gsubfn)
  library(maptools) #for writeSpatialShape
  library(rgeos) #apparently maptools needs this loaded
  library(dplyr) #for between() and %>%

## DEFINE PROJECTIONS ##
  latlong = CRS("+init=epsg:4326")
  stateplane = CRS("+init=epsg:2818")
  
## COLLAR DATA ##

  # raw data - collar locations and capture info
  locsraw <- read.csv("../DatabasesEtc/collardata-locsonly-equalsampling.csv", as.is = TRUE, header = TRUE)
  caploc <- read.csv("../DatabasesEtc/capture-locations.csv")
  
  # format date and use to define seasons
  locsraw$Date <- as.Date(locsraw$Date, "%Y-%m-%d")
  locsraw$Month <- as.numeric(format(as.POSIXlt(locsraw$Date), "%m"))
  locsraw$Season <- ifelse(locsraw$Month == 03, "March",
                       ifelse(locsraw$Month == 04, "April",
                              ifelse(locsraw$Month == 05, "May",
                                     ifelse(between(locsraw$Month, 06, 08), "Summer",
                                           ifelse(between(locsraw$Month, 09, 11), "Fall", 
                                                  "Winter")))))

  # remove ski hill elk
  locs <- locsraw %>%
    left_join(caploc, by = "AnimalID") %>%
    filter(Location != "Ski Hill")
  
  # sanity check (verify numbers are equal)
  length(which(caploc$Location == "Ski Hill"))
  length(unique(locsraw$AnimalID)) - length(unique(locs$AnimalID))
  
  # define sexes for for loops
  sexes <- list("Female", "Male")  


  
#### KDEs ####  
  
#### Winter (Dec - Feb) ####
  dat <- filter(locs, Season == "Winter")
  time <- "winter-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }


#### March ####
  dat <- filter(locs, Season == "March")
  time <- "march-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }


#### April ####
  dat <- filter(locs, Season == "April")
  time <- "april-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }



#### May ####
  dat <- filter(locs, Season == "May")
  time <- "may-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }



#### Calving (May 27 - Jun 1) ####
  dat <- locs %>%
    filter(Sex == "Female") %>%
    filter(Date >= "2014-05-27" & Date <= "2014-06-10" | 
             Date >= "2015-05-27" & Date <= "2015-06-10" )

  dt <- dat
  xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
  ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
  sp <- spTransform(ll,stateplane)
  kde <- kernelUD(sp, h="href", grid=5000)
  raster <- raster(kde)
  writeRaster(raster, filename="calving-Female.tif", format="GTiff", overwrite=TRUE)



#### Summer (Jun - Aug) ####
  dat <- filter(locs, Season == "Summer")
  time <- "summer-"


  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }



#### Archery (Sep 5 - Oct 19) ####
  dat <- locs %>%
    filter(Date >= "2014-09-05" & Date <= "2014-10-19" | 
             Date >= "2015-09-05" & Date <= "2015-10-19" )
  time <- "archery-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }


#### Rifle (Oct 24 - Nov 30) ####
  dat <- locs %>%
    filter(Date >= "2014-10-24" & Date <= "2014-11-30" | 
             Date >= "2015-10-24" & Date <= "2015-11-30" )
  time <- "rifle-"
  
  for (i in 1:length(sexes)){
    dt <- filter(dat, Sex == sexes[[i]])
    fn <- paste0(time, sexes[[i]], ".tif")
    xy <- data.frame("x"=dt$Long, "y"=dt$Lat)
    ll <- SpatialPointsDataFrame(xy, dt, proj4string = latlong)
    sp <- spTransform(ll,stateplane)
    kde <- kernelUD(sp, h="href", grid=5000)
    raster <- raster(kde)
    writeRaster(raster, filename=fn, format="GTiff", overwrite=TRUE)
  }
