options(scipen=999, stringsAsFactors=F)

library(rgdal)
library(raster)
library(pbapply)
library(data.table)

##read in pertinent layers
##forest class layer
forestClass <- raster("F:/Goode_FinalClassification_19_05pcnt_prj.tif")
coBounds <- readOGR("N:/generalGIS/backgroundData", "gadm2_nation")
##reproject both layers into a coordinates system which preserves area
##Mollweide (WGS84)
proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
projCoBounds <- spTransform(coBounds, proj)
projForest <- trim(projectRaster(forestClass, crs=crs(projCoBounds), res=9664, method="ngb"))


##########################################################################
##########################################################################
freqByPolyBounds <- function(bound, rast){
  #################
  #bound <- projCoBounds[projCoBounds$NAME_0==projCoBounds$NAME_0[1],]
  #rast <- projForest
  #boundC <- coBounds[coBounds$NAME_0=="United States",]
  #rastC <- forestClass
  #################

  print(bound$NAME_0)
  focalCounts <- as.data.frame(freq(mask(crop(rast, bound), bound), useNA="no"))
  #focalCountsC <- as.data.frame(freq(mask(crop(rastC, boundC), boundC), useNA="no"))
  colnames(focalCounts) <- c("class", "numCells")
  
  focalCounts$areaKM <- (focalCounts$numCells * res(rast)[1] * res(rast)[2]) / 1000
  focalCounts$coName <- bound$NAME_0
  focalCounts$coISO <- bound$ISO

  return(focalCounts)
}
##########################################################################
##########################################################################


zonalByCos <- pblapply(projCoBounds$NAME_0, function(coT){tryCatch(freqByPolyBounds(projCoBounds[projCoBounds$NAME_0==coT,], projForest), error=function(e) NULL)})
zonalByCosFullTab <- rbindlist(zonalByCos)
write.csv(zonalByCosFullTab, "F:/forestClassesByCountry.csv", row.names=F)




