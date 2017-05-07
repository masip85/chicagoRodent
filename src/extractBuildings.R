library(data.table)
library(rgdal)
library(sp)
# library(gpclib)
library(doMC)
library(foreach)
suppressWarnings(source("src/utils.R"))

if(!exists("verbose")) verbose = TRUE

# # Poligonos y edad de los edificios
shapefile <- readOGR("inputs/raw/Buildings/",
                     "buildings")
shapefile.converted <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
shapefile <- shapefile.converted
rm(shapefile.converted)
gc()

# # # # # Cargar todo el mapa de manera optimizada
# gpclibPermit()
kCluster(3)
years <- sort(unique(shapefile$YEAR_BUILT))
years <- years[-1] # elimino el 0
if (verbose == TRUE) file.remove("tmp/completingExtracBuildLoop.txt")
build.year.list <- foreach (i=years) %dopar% {
  start.time1 <- Sys.time()
  
  SPsubset <- shapefile[shapefile$YEAR_BUILT==i,]
  SPsubset@data$id = as.character(as.numeric(rownames(SPsubset@data)) + 1)
  # dtPolyById = fortify (SPsubset, region = "id")
  # dtBuildingsMap = inner_join(dtPolyById, SPsubset@data, by = "id")
  # El data table del mapa de polígonos,nos per
  dtCentroidBuildings <- as.data.table(cbind(coordinates(SPsubset),
                                             SPsubset@data$id))
  setnames(dtCentroidBuildings,c("long","lat","id"))
  dtBuildAge <- SPsubset@data[,c("YEAR_BUILT","ST_TYPE1","SHAPE_AREA","id")]
  
  
  dtBuildCentroidsMap <- as.data.table(merge(dtBuildAge,dtCentroidBuildings,
                                             by="id",all=FALSE))
  
  if (verbose ==TRUE){
  end.time1 <- Sys.time()
  time.taken1 <- end.time1 - start.time1
  line <- paste("Tiempo: : :",  toString(i)," : ", toString(time.taken1))
  line2 <- paste("\n",toString(nrow(dtBuildAge)),
                 toString(nrow(dtCentroidBuildings)),
                 toString(nrow(dtBuildCentroidsMap))
                 )
  write(line,file="tmp/completingExtracBuildLoop.txt",append=TRUE)
  write(line2,file="tmp/completingExtracBuildLoop.txt",append=TRUE)
  }
  
  dtBuildCentroidsMap
}
dtBuildCentroidsMap <- rbindlist(build.year.list)
saveRDS(dtBuildCentroidsMap,"inputs/preProcess/buildCenters.RData")



# write.table(build.centers,"buildCenters.csv",sep=";",dec=",",row.names =F)