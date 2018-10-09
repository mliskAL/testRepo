
##Imports libraries
library(sp)
library(rgdal)
library(proj4)
library(raster)
library(plotKML)

##Gets input variables
setwd("C:/Users/mlisk/Documents/MSCC/mbss_results")
polygon = readOGR(".", "test2Out")

##Sets projection
BNG<- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") # projection of coordinates

inSyst = proj4string(polygon)

file = spTransform(polygon, BNG)
inSyst2 = proj4string(file)

##Output location
setwd("C:/Users/mlisk/Documents/MSCC/mbss_results")
##Writes out kml file
writeOGR(file, dsn="testReal.kml", layer="testRealClust", driver="KML")

##########################################################################
##Multiple
inDir="M:/MSCC/analysisResults_Data/clustering/"
outDir="M:/MSCC/analysisResults_Data/clustering/"
setwd(inDir)
listPolys = list.files(pattern="_q0_16.shp")
sheds = unique(sapply(strsplit(listPolys, ".shp"), "[[", 1))

streamShps = lapply(sheds, readOGR, dsn=".")

##function form
convert2KML = function(shp){
  BNG = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") # projection of coordinates
  inSyst = proj4string(shp)
  
  file = spTransform(shp, BNG)
  #inSyst2 = proj4string(file)
  return(file)
}

streamKMLS = lapply(streamShps, convert2KML)

setwd(outDir)
mapply(function(shp, name){writeOGR(shp, dsn=paste(name, ".kml", sep=""), layer=name, driver="KML")}, shp=streamKMLS, name=sheds)
##########################################################################
##single raster
setwd("C:/Users/mlisk/Documents/junk/")
testRast = raster("M:/MSCC/analysisResults_Data/watershedsRasters/02060003/str_mask.tif")
file = projectRaster(testRast, crs=BNG)

KML(file, "testRast.kml")

