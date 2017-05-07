setwd("~/chicagoRodent")

##### -------------------- CARGA LIBRERIAS Y FUNCIONES --------------------#####  
# library(rjson)
library(data.table)
# Especifico mi uso de shift,ya que con raster library pierdo el shift de los DT
shift <- data.table::shift 
library(doMC)
library(foreach)
suppressWarnings(source("src/utils.R"))
suppressWarnings(source("src/extractCSV.R"))

##### ----------------------- ESTABLEZCO OPCIONES -------------------------#####
verbose = TRUE
options(datatable.verbose = verbose,stringsAsFactors = FALSE)

# La consola por defecto no muestra todos los dígitos
if (verbose == T) options(digits = 17) # Lat/Long Contiene 17 digitos

library(rgdal)
# library(ggmap)
library(shapefiles)
library(ggplot2)
library(sp)
# library(rbokeh)
# library(dplyr)
# library(maptools)
library(gpclib)
# library(leaflet)
library(raster)
library(doMC)
library(foreach)

##### --------------------- VARIABLES, LIMITES, ETC ------------------------#### 
# Max Cores
maxCores <<- 3
# Frontera de lo que se considera lugar problematico o recurrente
recurrentLimit <- 2
# tiempo que las ratas pueden haber estado anidando en edificio abandonado
abandonedBuildNestDays <- 365 
# tiempo que las ratas pueden haber estado anidando antes del avistamiento
rodentUnnoticedDays <- 30 
# Tiempo durante el cual el cebo hace efecto
baitEffectTime <- 90
# Tiempo durante el cual el restaurante llevaba teniendo un problema.
foodIProblemDays<- 365
foodIFixDays<- 30
# parametro entrada: resolucion cuadricula
resoObjetivo <- 0.5
##### ------------------------- DEFINICIONES CRS ---------------------------#### 
# Proyectadas: UTM special Chicago Open Data
crs_chicago_utm <- "+proj=tmerc +lat_0=36.66666666666666 
+lon_0=-88.33333333333333 +k=0.9999749999999999 +x_0=300000 +y_0=0 +datum=NAD83 
+units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"
# GeoRef: latLong WGS84.Utilizadas en Chicago Open Data y por defecto en leaflet.
#+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
crs_WGS84 <- "+init=epsg:4326"


##### --------------------- EXTRACCION DATOS CSV's ------------------------##### 
if (!file.exists("inputs/preProcess/abandonedBuildRodent.RData")){
  allData <- cargarCSVs()
  weather <- allData[[1]]
  foodI <- allData[[2]]
  restaurants <- allData[[3]]
  rodent <- allData[[4]]
  bait <- allData[[5]]
  abandonedBuild <- allData[[6]]
  abandonedBuildRodent <- allData[[7]]
  trash <- allData[[8]]
  waterStanding<- allData[[9]]
  dogFeces<- allData[[10]]
  trashes <- allData[[11]]
  constructTrash <- allData[[12]]
  
} else {
  weather <- readRDS("inputs/preProcess/weather.RData")
  foodI <- readRDS("inputs/preProcess/foodI.RData")
  restaurants <- readRDS("inputs/preProcess/restaurants.RData")
  rodent <- readRDS("inputs/preProcess/rodent.RData")
  bait <- readRDS("inputs/preProcess/baits.RData")
  abandonedBuild <- readRDS("inputs/preProcess/abandonedBuild.RData")
  abandonedBuildRodent <-readRDS("inputs/preProcess/abandonedBuildRodent.RData")
  trash <- readRDS("inputs/preProcess/trash.RData")
  waterStanding <- readRDS("inputs/preProcess/waterStanding.RData")
  dogFeces <- readRDS("inputs/preProcess/dogFeces.RData")
  trashes <- readRDS("inputs/preProcess/trashes.RData")
  constructTrash <-readRDS("inputs/preProcess/constructTrash.RData")
}

##### ---------------- COMPLETAMOS DATOS CON  SHAPEFILES ------------------#####
ifelse (!file.exists("inputs/preProcess/buildCenters.RData"),
        source("src/extractBuildings.R"),
        dtBuildCentroids <- readRDS(
          "inputs/preProcess/buildCenters.RData")[, ':='(long = as.numeric(long),
                                                         lat = as.numeric(lat))]
)
ifelse (!file.exists("inputs/preProcess/parkCenters.RData"),
        source("src/extractParks.R"),
        dtParkCentroids <- readRDS(
          "inputs/preProcess/parkCenters.RData")[, ':='(long = as.numeric(long),
                                                        lat = as.numeric(lat))]
)

##### -------------------- DATOS ENDOGENOS --------------------------------#####
# Creo nuevas tablas con datos "derivados" de las mismas
rodentRecurrentPlace <- rodent[!is.na(long) & !is.na(lat), list(nRatsHist = .N),
                               by=c("lat","long")][nRatsHist>recurrentLimit]

###### ------------- CREAMOS RASTER FILE Y SUS LINEAS ------------------- #####
# #  Obtencion líneas
# Obtenemos esquinas
frameCorners <- getCorners(rodent)
# Pasamos a rad
frameCorners <- deg2rad(frameCorners)
# Tamaño considerando esquinas ciudad : resolucion entrada
distCornersLat <- gcd.dist(frameCorners[1,long], frameCorners[1,lat],
                           frameCorners[1,long], frameCorners[2,lat]) # dist en lat = 41.92
distCornersLong <- gcd.dist(frameCorners[1,long], frameCorners[1,lat],
                            frameCorners[2,long], frameCorners[1,lat]) # dist en long = 26.37



# Numero cuadrados en lat y long
nLat <-  floor(distCornersLat /resoObjetivo)
nLong <- floor(distCornersLong/resoObjetivo)
totalSquares <- nLat * nLong

# resolucion final por redondeo floor
resObjetivoLat <- distCornersLat/nLat
resObjetivoLong <- distCornersLong/nLong

# vectores de lat/long
frameCorners <- rad2deg(frameCorners)
lat.vec <- seq(frameCorners[1,lat], frameCorners[2,lat],
               length.out = nLat)# n divisiones de resObjetivo Km
long.vec <-  seq(frameCorners[1, long], frameCorners[2, long], 
                 length.out = nLong)# n divisiones de resObjetivo Km

# Combinaciones todos los puntos desde vector de lat's y long's
gridPoints <- as.data.table(expand.grid(lat.vec,long.vec))
# gridPoints <- (coordinates(r))
setnames(gridPoints,c("lat","long"))

# puntos de izq a derecha lineas lat
# puntos de arr a abajo  lineas long
p.Sup <- gridPoints[order(lat)][1:nLong]
p.Inf <- gridPoints[order(-lat)][1:nLong]
p.Izq <- gridPoints[order(long)][1:nLat]
p.Der <- gridPoints[order(-long)][1:nLat]
l.vert <- rbind(p.Sup,p.Inf)[order(long)]
l.horz <- rbind(p.Izq,p.Der)[order(lat)]

# #  RASTER
# Creo capa cuadricula con libreria RASTER
r <- raster(xmn = frameCorners[2, long],ymx = frameCorners[2, lat],
            xmx = frameCorners[1, long],ymn = frameCorners[1, lat], 
            nrows = nLat, ncols = nLong)
# totalSquares=ncell(r) # check
values(r) <- matrix(1:totalSquares, nrow(r), ncol(r), byrow = FALSE)
# To returns: +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0
crs(r) <- CRS(crs_WGS84)



##### ------------------ POST PROCESADO DATOS -----------------------------#####
# 
weather[is.na(snowdepthm), snowdepthm := 0]
weather[is.na(precipm), precipm := 0]
# Elimino casos con datos mal introducidos
foodI <- foodI[!is.na(long) & !is.na(lat)]
restaurants <- restaurants[!is.na(long) & !is.na(lat)]
rodent <- rodent[!is.na(long) & !is.na(lat)]
bait <- bait [!is.na(long) & !is.na(lat)]
abandonedBuild <- abandonedBuild[ !is.na(long) & !is.na(lat)]
abandonedBuildRodent <- abandonedBuildRodent[!is.na(long) & !is.na(lat)]
trash <- trash[ !is.na(long) & !is.na(lat)]
waterStanding <- waterStanding[ !is.na(long) & !is.na(lat)]
dogFeces <- dogFeces[ !is.na(long) & !is.na(lat)]
trashes <- trashes[ !is.na(long) & !is.na(lat)]
constructTrash <- constructTrash[ !is.na(long) & !is.na(lat)]
dtBuildCentroids <- dtBuildCentroids[ !is.na(long) & !is.na(lat)]
dtParkCentroids <- dtParkCentroids[!is.na(long) & !is.na(lat)]

# Añado coordenadas UTM (X_Coord e Y_Coord a )
# rodent <- addUTMCoords(rodent)
foodI <- addUTMCoords(foodI)
restaurants <- addUTMCoords(restaurants)
# rodent - UTM venia ya incluido
bait <- addUTMCoords(bait)
# abandonedBuild
abandonedBuildRodent <- addUTMCoords(abandonedBuildRodent)
#trash <- addUTMCoords(trash)
#waterStanding <- addUTMCoords(waterStanding)
#dogFeces <- addUTMCoords(dogFeces)
trashes <- addUTMCoords(trashes)
constructTrash <- addUTMCoords(constructTrash)
dtBuildCentroids <- addUTMCoords(dtBuildCentroids)
dtParkCentroids <- addUTMCoords(dtParkCentroids)
rodentRecurrentPlace <- addUTMCoords(rodentRecurrentPlace)


# Obtengo el cuadrante(SQUARE) de cada dataset
foodI[, square := cellFromXY(r, foodI[, list(long, lat)])]
restaurants[, square := cellFromXY(r, restaurants[, list(long, lat)])]
rodent[, square := cellFromXY(r, rodent[, list(long, lat)])]
bait[, square := cellFromXY(r, bait[, list(long, lat)])]
abandonedBuild[, square := cellFromXY(r, abandonedBuild[, list(long, lat)])]
abandonedBuildRodent[, square := 
                       cellFromXY(r, abandonedBuildRodent[, list(long, lat)])]
trash[, square := cellFromXY(r, trash[, list(long, lat)])]
waterStanding[, square := cellFromXY(r, waterStanding[, list(long, lat)])]
dogFeces[, square := cellFromXY(r, dogFeces[, list(long, lat)])]
trashes[, square := cellFromXY(r, trashes[, list(long, lat)])]
constructTrash[, square := cellFromXY(r, constructTrash[, list(long, lat)])]
dtBuildCentroids[, square := cellFromXY(r, dtBuildCentroids[, list(long, lat)])]
dtParkCentroids[, square := cellFromXY(r, dtParkCentroids[, list(long, lat)])]
rodentRecurrentPlace[, square := 
                       cellFromXY(r, rodentRecurrentPlace[, list(long, lat)])]
# Edificios abandonados con ratas
# rodentRecurrentPlace <- rodentRecurrentPlace[, nRatsHistF := cut(nRatsHist,breaks = c(1,3,7,10))]

#  Imaginamos que las ratas llevan x tiempo ahi dando problemas
abandonedBuildRodent <- setnames(abandonedBuildRodent, "fecha", "fechaFin")
abandonedBuildRodent <- abandonedBuildRodent[, fechaIni := fechaFin - 
                                               abandonedBuildNestDays]

bait <-  setnames(bait, "fecha", "fechaIni")
bait <- bait[, fechaFin := fechaIni + baitEffectTime]

foodI <- foodI[, fechaFin := fechaIni + foodIFixDays]
foodI <- foodI[, fechaIni := fechaIni - foodIProblemDays]



##### ------------------ ADICION INFO PROXIMA AL ROEDOR -------------------#####
rodent <- rodent[order(square)]
# rodent[square %in% adjacent(r,square,directions = 8,include = T,pairs=FALSE),sum(nRats)]

# Adicion datos a apariciones de roedores:elemento mas cercano,tipo del mismo,
# Solo mirare en celdas adyacentes  
squareUsed <- rodent[!is.na(square),unique(square)]
options(datatable.verbose = FALSE)
start.time0 <- Sys.time()
if (verbose == TRUE) file.remove("tmp/completingSquareLoop.txt")
myCluster(3)
emptyColumnDouble <- rep(as.numeric(NA),nrow(rodent))
emptyColumnString <- rep(as.character(NA),nrow(rodent))
emptyColumnLogical <- rep(as.logical(NA),nrow(rodent))
# Inicializo los campos nuevos. Si no se inicializa así realiza un coerce a
# logical de la columna de manera automatica.
rodent[,':='(closestBuild = emptyColumnDouble, 
             closestBuildType = emptyColumnString, 
             closestBuildYear = emptyColumnDouble, 
             closestAbandonBuild = emptyColumnDouble, 
             closestAbandonBuildRodent = emptyColumnDouble,
             closestBait = emptyColumnDouble, closestBaitNumber = emptyColumnDouble, 
             closestRecurrent = emptyColumnDouble, 
             closestRecurrentNRats = emptyColumnDouble,
             closestPark = emptyColumnDouble, closestParkAcres = emptyColumnDouble, 
             closestRestaurant = emptyColumnDouble, 
             closestFoodI = emptyColumnDouble, 
             closestFoodI_R1 = emptyColumnLogical,
             closestFoodI_R2 = emptyColumnLogical,
             closestFoodI_R3 = emptyColumnLogical, 
             closestTrash = emptyColumnDouble, 
             closestContructTrash = emptyColumnDouble, 
             closestFece = emptyColumnDouble, 
             closestWater = emptyColumnDouble) ]



square.distInfo <- foreach (j = squareUsed) %dopar% {
  if (verbose == TRUE) start.time1 <- Sys.time()
  
  squareList <-   adjacent(r,j,
                           directions = 8, include = T, pairs = FALSE)
  indexRodent <- rodent[square == j,which = TRUE]
  
  if(!isEmpty(indexRodent)){
    
    rodent.list <- foreach (i = indexRodent) %do% {
      # if (verbose == TRUE) print(paste0("Ini rodent ------",toString(i)))
      # # Punto donde aparece el roedor
      pt <- as.vector(unlist(rodent[i,list(xCoord,yCoord)]))
      
      # Primero informacion de elementos atemporales:
      #  Edificios
      indexes <- dtBuildCentroids[square %in% squareList, which = TRUE ]
      if(!isEmpty(indexes)){
        pts = as.matrix(dtBuildCentroids[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, ':='(closestBuild = feetToMeters(closestDist),
                       closestBuildType = dtBuildCentroids[closestIndex,ST_TYPE1],
                       closestBuildYear = dtBuildCentroids[closestIndex,YEAR_BUILT])]
      } else{
        # rodent[i, ':='(closestBuild = NA,
        #                closestBuildType = NA,
        #                closestBuildYear = NA)]
      }
      # Edificio abandonado
      indexes <- abandonedBuild[square %in% squareList, which = TRUE ]
      if(!isEmpty(indexes)){
        pts = as.matrix(abandonedBuild[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestAbandonBuild := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestAbandonBuild := NA]
      }
      # Puntos recurrentes
      indexes <- rodentRecurrentPlace[square %in% squareList, which = TRUE ]
      if(!isEmpty(indexes)){
        pts = as.matrix(rodentRecurrentPlace[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, ':='(closestRecurrent = feetToMeters(closestDist),
                       closestRecurrentNRats =
                         rodentRecurrentPlace[closestIndex,nRatsHist])]
      } else{
        # rodent[i, ':='(closestRecurrent = NA,
        #                closestRecurrentNRats = NA)]
        
      }
      # Parques
      indexes <- dtParkCentroids[square %in% squareList, which = TRUE ]
      if(!isEmpty(indexes)){
        pts = as.matrix(dtParkCentroids[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i,  ':='(closestPark = feetToMeters(closestDist),
                        closestParkAcres = dtParkCentroids[closestIndex,Acres])]
      } else{
        # rodent[i,  ':='(closestPark = NA,
        #                 closestParkAcres = NA)]
        
      }
      
      # A continuacion informacion de elementos temporales:
      # Restaurantes
      indexes <- restaurants[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                             & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(restaurants[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestRestaurant := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestRestaurantDist := NA]
      }
      # Edificios abandonados con ratas
      indexes <-
        abandonedBuildRodent[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                             & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(abandonedBuildRodent[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestAbandonBuildRodent := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestAbandonBuildDist := NA]
      }
      # Trashes
      indexes <- trashes[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                         & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(trashes[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestTrash := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestTrash := NA]
      }
      # Dog Feces
      indexes <- dogFeces[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                          & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(dogFeces[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestFece := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestFece := NA]
      }
      # Baits
      indexes <- bait[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                      & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(bait[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, ':='(closestBait = feetToMeters(closestDist),
                       closestBaitNumber = bait[closestIndex, baits])]
      } else{
        # rodent[i, ':='(closestBait = NA,
        #                closestBaitNumber = NA )]
      }
      # Contruct Trash
      indexes <- constructTrash[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                                & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(constructTrash[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestContructTrash := feetToMeters(closestDist)]
      } else{
        # rodent[i, closestContructTrash := NA]
      }
      # Water Standing
      indexes <- waterStanding[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                               & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(waterStanding[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, closestWater:= feetToMeters(closestDist)]
      } else{
        # rodent[i, closestWater:= NA]
        
      }
      # foodI
      indexes <- foodI[rodent[i, fecha] > fechaIni & rodent[i, fecha] < fechaFin
                       & square %in% squareList,which = TRUE]
      if(!isEmpty(indexes)){
        pts = as.matrix(foodI[indexes,list(xCoord,yCoord)])
        dist.vector <- cart.dist(pts,pt)
        closestDist <- min(dist.vector)
        closestIndex <- indexes[which.min(dist.vector)]
        if (verbose == TRUE) print(closestDist)
        rodent[i, ':='(closestFoodI = feetToMeters(closestDist),
                       closestFoodI_R1 = foodI[closestIndex, rodent1],
                       closestFoodI_R2 = foodI[closestIndex, rodent2],
                       closestFoodI_R3 = foodI[closestIndex, rodent3])]
      } else{
        # rodent[i, ':='(closestFoodI = NA,
        #                closestFoodI_R1 = NA,
        #                closestFoodI_R2 = NA,
        #                closestFoodI_R3 = NA)]
        
      }
      
      line <- toString(rodent[i,])
      write(line,file="tmp/completingSquareLoop2.txt",append=TRUE)
    }
  } # Cierro bucle roedores de un determinado square
  
  
  if (verbose == T){
    end.time1 <- Sys.time()
    time.taken1 <- end.time1 - start.time1
    line <- paste("Tiempo: : :",  toString(j)," : ", toString(time.taken1))
    write(line,file="tmp/completingSquareLoop.txt",append=TRUE)
  }
  return(rodent[square == j])
} # Cierro bucle square
end.time0 <- Sys.time()
time.taken0 <- end.time0 - start.time0

ratsDataset <- rbindlist(square.distInfo)

##### ----------- DELIMITACIÓN TEMPORAL ----------##### 
# Rango de fechas
maxDate <- min(max(unique(rodent[, fecha])), max(unique(restaurants[, fecha])), 
               max(unique(foodI[, fechaIni])), max(unique(abandonedBuildRodent[, fecha])),
               max(unique(trash[, fechaIni])), max(unique(constructTrash[, fecha])),
               max(unique(weather[, fecha])))


minDate <- max(min(unique(rodent[, fecha])), min(unique(restaurants[, fecha])), 
               min(unique(foodI[, fechaIni])), min(unique(abandonedBuildRodent[, fecha])),
               min(unique(trash[, fechaIni])), min(unique(constructTrash[, fecha])),
               min(unique(weather[, fecha])))

# rangeDates <- range(unique(rodent[, fecha]))
rangeDates <- c(minDate,maxDate)

##### ----------- AGRUPACION SEMANAL DE LA INFO PRÓXIMA A ROEDOR ----------##### 

ratsDataset <- addYearMonthWeek(ratsDataset)

sq_cols <- paste0(names(ratsDataset),"_sq")
sq_cols <- sq_cols[!(sq_cols %in% c("fecha_sq", "year_sq",
                                    "month_sq", "week_sq","square_sq"))]
# medias distancias por "year","week","square" 
ratsDataset <- ratsDataset[,list(
  mean(lat,na.rm = T),
  mean(long,na.rm = T),
  mean(xCoord, na.rm = T),
  mean(yCoord, na.rm = T),
  sum(nRats, na.rm = T),
  mean(closestBuild, na.rm = T),
  names(which.max(table(closestBuildType))), # me quedo tipo calle mas frecuente
  mean(closestBuildYear, na.rm = T),
  mean(closestAbandonBuild, na.rm = T),
  mean(closestAbandonBuildRodent, na.rm = T),
  mean(closestBait, na.rm = T),
  mean(closestBaitNumber, na.rm = T),
  mean(closestRecurrent, na.rm = T),
  mean(closestRecurrentNRats, na.rm = T),
  mean(closestPark, na.rm = T),
  mean(closestParkAcres, na.rm = T),
  mean(closestRestaurant, na.rm = T),
  mean(closestFoodI, na.rm = T),
  mean(closestFoodI_R1, na.rm = T),
  mean(closestFoodI_R2, na.rm = T),
  mean(closestFoodI_R3, na.rm = T),
  mean(closestTrash, na.rm = T),
  mean(closestContructTrash, na.rm = T),
  mean(closestFece, na.rm = T),
  mean(closestWater, na.rm = T),
  .N), by = c("year","week","square")]
setnames(ratsDataset,c("year","week","square",sq_cols,"N_sq"))
# ratsDataset2 <- ratsDataset2[,list(year, week, square, lat_sq, long_sq, 
#                                    xCoord_sq,yCoord_sq, nRats_sq)]

##### ----------------- AGRUPACION POR SQ&UD.TIEMPO -----------------------##### 
# Caracteristicas square o square week
restaurants <- restaurants[,list(fechaIni,fechaFin,square)]
restaurants <- restaurants[, id := c(1:nrow(restaurants))]
restaurants <- restaurants[fechaIni < fechaFin,
                           list(square = square, fecha=seq(fechaIni, fechaFin, 
                                                           by="week")),
                           by="id"]
restaurants <- addYearMonthWeek(restaurants)
restaurants_sq_w <- restaurants[,  list(restaurants_sq_w = .N), 
                                by = c("square", "week", "year")]

abandonedBuildRodent <- abandonedBuildRodent[, list(fechaIni,fechaFin,square)]
abandonedBuildRodent[, id := c(1:nrow(abandonedBuildRodent))]
abandonedBuildRodent <- abandonedBuildRodent[fechaIni < fechaFin,
                                             list(square = square, 
                                                  fecha=seq(fechaIni, fechaFin, 
                                                            by="week")),
                                             by="id"]
abandonedBuildRodent <- addYearMonthWeek(abandonedBuildRodent)
abandonedBuildRodent_sq_w <- 
  abandonedBuildRodent[, list(abandonedBuildRodent_sq_w = .N),
                       by = c("square", "week", "year")]

dogFeces <- dogFeces[,list(fechaIni,fechaFin,square)]
dogFeces <- dogFeces[, id := c(1:nrow(dogFeces))]
dogFeces <- dogFeces[fechaIni < fechaFin,
                     list(square = square, fecha=seq(fechaIni, fechaFin, 
                                                     by="week")),
                     by="id"]
dogFeces <- addYearMonthWeek(dogFeces)
dogFeces_sq_w <- dogFeces[, list(dogFeces_sq_w = .N), 
                          by = c("square", "week", "year")]

bait <- bait[,list(fechaIni,fechaFin,square)]
bait <- bait[, id := c(1:nrow(bait))]
bait <- bait[fechaIni < fechaFin,
             list(square = square, fecha=seq(fechaIni, fechaFin, 
                                             by="week")),
             by="id"]
bait <- addYearMonthWeek(bait)
bait_sq_w <- bait[, list(bait_sq_w = .N), by = c("square", "week", "year")]

constructTrash <- constructTrash[,list(fechaIni,fechaFin,square)]
constructTrash <- constructTrash[, id := c(1:nrow(constructTrash))]
constructTrash <- constructTrash[fechaIni < fechaFin,
                                 list(square = square, fecha=seq(fechaIni, fechaFin, 
                                                                 by="week")),
                                 by="id"]
constructTrash <- addYearMonthWeek(constructTrash)
constructTrash_sq_w <- constructTrash[, list(constructTrash_sq_w = .N),
                                      by = c("square", "week", "year")]

trashes <- trashes[,list(fechaIni,fechaFin,square)]
trashes <- trashes[, id := c(1:nrow(trashes))]
trashes <- trashes[fechaIni < fechaFin,
                   list(square = square, fecha=seq(fechaIni, fechaFin, 
                                                   by="week")),
                   by="id"]
trashes <- addYearMonthWeek(trashes)
trashes_sq_w <- trashes[, list(trashes_sq_w = .N),
                        by = c("square", "week", "year")]

waterStanding <- waterStanding[, list(fechaIni, fechaFin, square)]
waterStanding <- waterStanding[, id := c(1:nrow(waterStanding))]
waterStanding <- waterStanding[fechaIni < fechaFin,
                               list(square = square, fecha=seq(fechaIni, fechaFin, 
                                                               by="week")),
                               by="id"]
waterStanding <- addYearMonthWeek(waterStanding)
waterStanding_sq_w <- waterStanding[, list(waterStanding_sq_w = .N), 
                                    by = c("square", "week", "year")]

rodent <- addYearMonthWeek(rodent)
rodent_sq_w <- rodent[,list(rodent_sq_w = sum(nRats)), by = c("square", "week", "year")]

weather <- addYearMonthWeek(weather)
weather_w  <- weather[,list(maxtempm = mean(maxtempm),mintempm=mean(mintempm),
                            meanwindspdm = mean(meanwindspdm),
                            precipm = sum(precipm), snowdepthm = sum(snowdepthm), 
                            maxhumidity = mean(maxhumidity),
                            minhumidity = mean(minhumidity)), by =c("week",
                                                                        "year")]

abandonedBuild_sq <- abandonedBuild[,  list(aBuild_sq = .N), by = "square"]


##### ----------------------- UNION FUENTES DATOS -------------------------#####
# dt.squareUsed <- data.table(squareUsed = squareUsed)
# dt.rangeDates <- data.table(rangeDates = seq(rangeDates[1],rangeDates[2], by = 1))
# dt.allSqRDates<- merge(dt.rangeDates,dt.squareUsed,by=NULL )
dt.allSqRDates<- as.data.table(expand.grid(seq(rangeDates[1],rangeDates[2], by = 1),
                                           squareUsed))
dt.allSqRDates <- setnames(dt.allSqRDates,c("fecha","square"))
dt.allSqRDates <- addYearMonthWeek(dt.allSqRDates)
dt.allSqRDates <- dt.allSqRDates[, ':='(fecha = NULL,month = NULL)]
setkey(dt.allSqRDates, NULL)
dt.allSqRDates <- unique(dt.allSqRDates)



res <- merge(dt.allSqRDates, weather_w,allow.cartesian = TRUE,
             by = c("year","week"), all.x = TRUE)
res <- merge(res, abandonedBuild_sq,allow.cartesian = TRUE,
             by = c("square"), all.x = TRUE)
res <- merge(res, restaurants_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, waterStanding_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, trashes_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, constructTrash_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, dogFeces_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, abandonedBuildRodent_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, bait_sq_w,by = c("year","week","square"),all.x = T)
res <- merge(res, rodent_sq_w,by = c("year","week","square"),all.x = T)



##### -------------------------- CONVOLUCIÓN -----------------------------#####
# Primero evaluo los sq vacios necesarios para mantener orden del raster
square_empty <- (1:totalSquares)[!( (1:totalSquares)  %in% res[, unique(square)] )]
square_empty <- as.data.table(square_empty)
setnames(square_empty, "square_empty", "square")

# resNames <- names(res)[-(1:13)]
# Iterator is time unit string (year-week)
res[, iterator :=  paste(year,week,sep = "_")]
res <- res[order(iterator)]
setcolorder(res,neworder = c(3,length(res),1,2,4:(length(res) - 1)))
# Mascara de convolucion
maskMatrix  = matrix(c(1/4,1/2,1/4,1/2,1,1/2,1/4,1/2,1/4), nrow = 3)
for (i in 13:length(res)){ # las primeras 11 columnas solo hay info del tiempo 
  colName <- names(res)[i]
  print(colName)
  square_empty <- square_empty[,list(square)] # reset for next loop iteration
  square_empty[, (colName) := NA_integer_]
  
  # Por fecha voy rellenando la columna de res con sus square empty
  iterator.list <- foreach (j = res[,unique(iterator)]) %do% {
    square_empty[, iterator := j]
    print(j)
    # en columna 3: square. en columna i: variable de iteracion bucle
    allRowsColI <- rbind(square_empty, res[iterator == j,c("square",colName,"iterator"),with = FALSE])
    completeVector <- allRowsColI[,get(colName)]
    values(r) <- matrix(completeVector, nrow(r), ncol(r), byrow = FALSE)
    # Convolución
    r = focal(r, w = maskMatrix, fun = "sum", na.rm = T, pad = TRUE,
              padValue = 0)
    # Extraigo convolucion y añado esos datos a la tabla res
    allRowsColI[, (paste0(colName,"_conv")) := values(r)]
    return(allRowsColI)
  }
  # Todos los valores "convolucionados" para todo square y ud. de tiempo
  allRowsColI_Conv <- rbindlist(iterator.list)[,c("square","iterator",
                                                  paste0(colName,"_conv")),
                                               with = FALSE]
  # Esos valores residiran en la tabla principal junto a los originales en los 
  # square y uds. de tiempo orginales.
  res <- merge(res, allRowsColI_Conv[,c("square","iterator",paste0(colName,"_conv")),
                                     with = FALSE],
               by = c("square","iterator"), all.x = TRUE)
  
}

##### ----------------------- ratsDataset2  -------------------------#####
# Aquí vendría la parte de analítica con este dataset 
ratsDataset
# Si juntamos los datos de ratas con el resto que incluye todas las posibilidades
# square-week-year, quedan muchas distancias NA. ¿Que hacemos con las distancias
# minimas de datos de rata que en realidad son NA? ¿Ponerle un número muy alto?
ratsDataset2 <- merge(res, ratsDataset, by =c("square", "week", "year"), 
                      all.x = TRUE)
##### -----------------------     res      --------------------------#####
# Con este dataset se realizaria el ML
res[is.na(res)] <- 0

# Ahora laggeamos variables. En este caso es más facil traer a presente 
# dato del futuro: target
target <- c("rodent_sq_w")
resNames <- names(res)[-(1:11)]
# ¿Que elementos podemos presumir que permanecerán iguales de un mes a otro?
remain <- resNames[c(1, 2, 5, 10, 11, 14)]
toLead <- c(target,remain)

res <- DT.getLeadVars2(dt = res, vars = toLead, k = 1, removeOriginal = FALSE,
                        fill = "NA")
res <- res[complete.cases(res)]















pos <- adjacent(r,1839,directions = 8,include = T,pairs=FALSE)
r[pos]


square_empty <- (1:totalSquares)[!( (1:totalSquares)  %in% res[, unique(square)] )]
square_empty <- as.data.table(square_empty)
setnames(square_empty, "square_empty", "square")
# square_empty[,list(long=xyFromCell(r,square_empty)[1],lat=xyFromCell(r,square)[2])]
square_empty[,':='(longCell = xyFromCell(r, square)[1,1],
                   latCell = xyFromCell(r,square)[1,2],
                   YEAR_BUILT_Mean = NA, SHAPE_AREA_Mean = NA, ST_TYPE = NA)]
rbind(dt, square_empty)









# Categorizo los edificios de la zona
# TODO: Se podria asignar una anchura a cada tipo de calle.
# Así tendriamos una variable que nos diría algo más sobre la zona, Porque 
# quedarse con el más frecuente puede no ser representativo.
aux <- dtBuildCentroids[, list(nKindST = .N), 
                        by = c("ST_TYPE1","square")][order(-nKindST),
                                                     list(ST_TYPE=ST_TYPE1[1]),
                                                     by = "square"]
dtBuildSq <- dtBuildCentroids[,list(longCell = xyFromCell(r,square)[1],
                                    latCell  = xyFromCell(r,square)[2],
                                    YEAR_BUILT_Mean = mean(YEAR_BUILT,
                                                           na.rm=T),
                                    SHAPE_AREA_Mean = mean(SHAPE_AREA,
                                                           na.rm = T)),
                              by="square"]
dtBuildSq <- merge(dtBuildSq, aux, by= "square",all.x=T)
# Si quiero rellenar los datos del raster, necesito poner NA 
# en celdas donde no tengo datos para plotear:
fillEmptySquares <- function( dt = NULL){
  square_empty <- (1:totalSquares)[!( (1:totalSquares)  %in% dt[, square] )]
  square_empty <- as.data.table(square_empty)
  setnames(square_empty, "square_empty", "square")
  # square_empty[,list(long=xyFromCell(r,square_empty)[1],lat=xyFromCell(r,square)[2])]
  square_empty[,':='(longCell = xyFromCell(r, square)[1,1],
                     latCell = xyFromCell(r,square)[1,2],
                     YEAR_BUILT_Mean = NA, SHAPE_AREA_Mean = NA, ST_TYPE = NA)]
  rbind(dt, square_empty)
}
dtBuildSq <- fillEmptySquares(dtBuildSq)[order(square)]

# Roedores 
rodentSq <- rodent[, list(nRats = sum(nRats, na.rm = T),
                          longCell = xyFromCell(r, square)[1,1],
                          latCell = xyFromCell(r, square)[1,2], .N), 
                   by = c("square", "fecha")][order(-N)]


##### 

squareList <- adjacent(r,rodent[i,square],directions = 8,include = T,pairs=F) 
# dist=map.dist(rodent[i,long],rodent[i,lat],dtBuildCentroidsMap[j,long],dtBuildCentroidsMap[j,lat])



qq <- spDistsN1(pts = mm2,pt = pt,longlat = FALSE)



distancesBuildings

# Cebos
baitsSq <- baits[,list(nBaits=sum(baits,na.rm=T),lat=mean(lat,na.rm=T),
                       long=mean(long,na.rm=T), longCell=xyFromCell(r,square)[1],
                       latCell=xyFromCell(r,square)[2]),by=c("square","fecha")]
# Restaurants
restaurants <- restaurants[fechaFin>rFechas[1]] # Filtro por fecha INI de Rodent
restaurants2 <- expandir(restaurants)
setkey(restaurants2,NULL)
restaurants2 <- unique(restaurants2)
restaurantsSq <- restaurants2[,list(lat=mean(lat,na.rm=T),long=mean(long,na.rm=T),
                                    longCell=xyFromCell(r,square)[1],latCell=xyFromCell(r,square)[2],nRestaurants=.N),by=c("square","fecha")]

fillValuesRestaurants <- function(dt=NULL){
  totalComb <- merge((1:totalSquares),IntToDate(rFechas[1]:rFechas[2]),all=TRUE)
  dt_absence <- totalComb[!(totalComb  %in% dt[,square] )]
  
  square_empty <- (1:totalSquares)[!( (1:totalSquares)  %in% dt[,square] )]
  fechas_empty <- IntToDate((rFechas[1]:rFechas[2])[!( (rFechas[1]:rFechas[2])  %in% unique(dt[,fecha]) )])
  dt2 <- as.data.table(merge(fechas_empty,square_empty,all=TRUE))[,list(fecha=x,square=y)]
  dt2[,':='(lat=NA,long=NA,longCell=xyFromCell(r,square)[1],latCell=xyFromCell(r,square)[2],nRestaurants=NA)]
  rbind(dt,dt2)
}
restaurantsSq <- fillValuesRestaurants(restaurantsSq)

# Inspecciones 
foodI <- foodI[order(-fechaIni)][,fechaFin:=shift(fechaIni,n = 1),by=c("lat","long")]
foodI <- foodI[fechaFin>rFechas[1]] # Filtro por fecha INI de Rodent
foodI[!(is.na(fechaFin) & !is.na(fechaIni))] # si ambos periodos son na, no lo queremos
setkey(foodI,NULL)
foodI <- unique(foodI)
foodI2 <- expandir(foodI)
foodISq <- foodI2[,list(lat=mean(lat,na.rm=T),long=mean(long,na.rm=T),
                        longCell=xyFromCell(r,square)[1],latCell=xyFromCell(r,square)[2],
                        nRodent1=sum(rodent1),nRodent2=sum(rodent2),nRodent3=sum(rodent3))
                  ,by=c("square","fecha")] 

fillValuesFoodI <- function(dt=NULL){
  square_empty <- (1:totalSquares)[!( (1:totalSquares)  %in% dt[,square] )]
  fechas_empty <- IntToDate((rFechas[1]:rFechas[2])[!( (rFechas[1]:rFechas[2])  %in% unique(dt[,fecha]) )])
  dt2 <- as.data.table(merge(fechas_empty,square_empty,all=TRUE))[,list(fecha=x,square=y)]
  dt2[,':='(lat=NA,long=NA,longCell=xyFromCell(r,square)[1],latCell=xyFromCell(r,square)[2],nRodent1=NA,nRodent2=NA,nRodent3=NA)]
  rbind(dt,dt2)
}
foodISq <- fillValuesFoodI(foodISq)


foodISq[is.na(nRodent1),nRodent1:=0]
foodISq[is.na(nRodent2),nRodent2:=0]
foodISq[is.na(nRodent3),nRodent3:=0]


# los periodos seran FI=NA FF=ALGO, FI=ALGO FF=ALGO, ... FI=ALGO, FF=NA
baits[,monthFin:=format(fechaFin,"%m")]
baits[,monthIni:=format(fechaIni,"%m")]
rodent[,monthFin:=format(fechaFin,"%m")]
rodent[,monthIni:=format(fechaIni,"%m")]



# dtBuildSq <- dtBuildSq[!is.na(dtBuildSq[,square])]
# Valor de antigüedad de los edificios al raster
values(r) <- dtBuildSq[ !is.na(square), YEAR_BUILT_Mean ]
# Si se quiere suavizar los valores, matriz de convolucion:
maskMatrix  = matrix(c(1/4,1/2,1/4,1/2,1,1/2,1/4,1/2,1/4), nrow = 3) # This defines the 3x3 moving window 
# Convolución
r = focal(r, w = maskMatrix, fun = "sum",na.rm = T)
matrix(1:totalSquares, nrow(r), ncol(r), byrow = FALSE)

# Celdas adyacentes
# adjacent(r,5,directions = 8,include = T,pairs=F) 
pos <- adjacent(r,1839,directions = 8,include = T,pairs=FALSE)
r[pos]



# rasterstack <- addLayer(rasterlayer, rasterLayer, rasterLayer) 
# 
# lvar: integer > 0 (default=3). To select the ’level variable’ (3rd dimension variable) to use, if the file has 4 dimensions (e.g. depth instead of time)
# level: integer > 0 (default=1). To select the ’level’ (4th dimension variable) to use, if the file has 4 dimensions, e.g. to create a RasterBrick of weather over time at a certain height.
# # 12 values of irradiation, 1 for each month
# G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
# # RasterBrick with 12 layers based on G0dm + noise
# r <- raster(nc=10, nr=10)
# s <- brick(lapply(1:12, function(x) setValues(r, G0dm[x]+100*rnorm(ncell(r)) )))
# # time
# tm <- seq(as.Date('2010-01-15'), as.Date('2010-12-15'), 'month')
# s <- setZ(s, tm, 'months')
# # library(zoo)
# # x <- zApply(s, by=as.yearqtr, fun=mean, name='quarters')
# 




