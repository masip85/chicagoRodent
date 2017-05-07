# LAG - Laggeo de variables ------------------------------
# Lag de cualquier variable pasada por parametro vars
getLagVars <- function(dt = NULL, vars = NULL, k = 1,removeOriginal = FALSE){
  dt <- dt[order(square, year, week)]
  for (var in vars){
    lagVar <- paste0("lag",k,"_",var)
    dt[,(lagVar) := shift(get(var), n = k, fill = get(var)[1]  ), 
       by = "square"]
    if (removeOriginal) dt <- DT.dropColumns(dt, var)
  }
  return(dt)
}
DT.getLagVars2 <- function(dt = NULL, vars = NULL, k = 1,removeOriginal = FALSE, 
                           fill = "NA"){
  dt <- dt[order(ncodpers, fecha_dato)]
  
  # Aparte de los vars que trato en paralelo, al final quedan los .init foreach:
  remainVars <- names(dt)
  if (removeOriginal) remainVars <- names(dt)[!(names(dt) %in% vars)]
  
  registerDoMC(min(detectCores()-1,length(vars), maxCores))
  result <- foreach(var = vars, .init = dt[,remainVars,with = FALSE],
                    .combine = cbind.data.frame, .inorder = TRUE) %dopar%{ 
                      lagVar <- paste0("lag",k,"_",var)
                      print(lagVar)
                      
                      # Podemos rellenar los bordes donde no hay dato de lag con el primer valor 
                      # existente, o rellenando con NA
                      if (fill =="first"){
                        varOut <- dt[,(lagVar) := shift(get(var), n = k, fill = get(var)[1]  ), 
                                     by = "ncodpers"][,lagVar, with = FALSE]
                      } else{
                        varOut <- dt[,(lagVar) := shift(get(var), n = k, fill = NA  ),
                                     by = "ncodpers"][,lagVar, with = FALSE]
                      }
                      
                      return (varOut)
                    }
  if (removeOriginal) result <- DT.dropColumns(result, vars)
  return(result)
}
DT.getLeadVars2 <- function(dt = NULL, vars = NULL, k = 1,removeOriginal = FALSE, 
                           fill = "NA"){
  
  dt <- dt[order(square, year, week)]
  # Aparte de los vars que trato en paralelo, al final quedan los .init foreach:
  remainVars <- names(dt)
  if (removeOriginal) remainVars <- names(dt)[!(names(dt) %in% vars)]
  
  registerDoMC(min(detectCores()-1,length(vars), maxCores))
  result <- foreach(var = vars, .init = dt[,remainVars,with = FALSE],
                    .combine = cbind.data.frame, .inorder = TRUE) %do%{ 
                      leadVar <- paste0("lead",k,"_",var)
                      print(leadVar)
                      
                      # Podemos rellenar los bordes donde no hay dato de lead con el primer valor 
                      # existente, o rellenando con NA
                      if (fill =="first"){
                        varOut <- dt[,(leadVar) := shift(get(var), 
                                                         n = k, 
                                                         fill = get(var)[length(get(var))],
                                                         type = "lead" ), 
                                     by = "square"][,leadVar, with = FALSE]
                      } else{
                        varOut <- dt[,(leadVar) := shift(get(var), 
                                                         n = k, 
                                                         fill = NA, 
                                                         type = "lead" ),
                                     by = "square"][,leadVar, with = FALSE]
                      }
                      
                      return (varOut)
                    }
  if (removeOriginal) result <- DT.dropColumns(result, vars)
  return(result)
}

DT.dropColumns<- function(dt = NULL, colsToDrop = NULL){
  dt[,(colsToDrop) := NULL]
  return(dt)
}

kCluster <- function(ncores){
  myTry(registerDoMC(ncores))
}

addYearMonthWeek <- function(dt = NULL){
  dt[,':='(year = as.integer(format(fecha,"%Y")),
           month = as.integer(format(fecha,"%m")),
           week = as.integer(format(fecha,"%V")))]
  dt
}

expandir <- function(dtIn=NULL){
  by <- 1000
  dt <- copy(dtIn)
  setnames(dt,c("fechaIni","fechaFin"),c("ini2","fin2"))
  prom.list <- foreach(j=seq(1,nrow(dt),by)) %dopar% {
    tmp <- dt[j:(j+by-1),]
    # tmp <- tmp[is.na(sku)==F,]
    fmin <- min(tmp$ini2,na.rm=T)
    fmax <- max(tmp$fin2,na.rm=T)
    vf <- fmin:fmax
    tmp2 <- as.data.table(merge.data.frame(tmp,vf,all=T)) 
    tmp2 <- tmp2[ini2<=y & fin2>=y,][,':='(fechaIni=NULL,fechaFin=NULL)]
    tmp2 <- setnames(tmp2,"y","fecha")
    tmp2 <- tmp2[order(fecha),]
    tmp2 <- tmp2[,fecha:=IntToDate(fecha)] 
    tmp2
  }
  dt <- rbindlist(prom.list)
  dt
}

IntToDate <- function(i){ as.Date("1970/01/01")+i }

feetToMeters <- function (x) x * 0.3048;

addUTMCoords <- function(dt){
  
  spCoord <- SpatialPointsDataFrame(coords = dt[, list(long,lat)], 
                                    proj4string = CRS(crs_WGS84),
                                    data = data.table(c(1:nrow(dt))) )
  coord.converted <- as.data.table(
    coordinates(spTransform(spCoord, CRS(crs_chicago_utm))))
  setnames(coord.converted, c("xCoord", "yCoord"))
  cbind(dt, coord.converted)
  
}

# recibe dt con latitude & longitude
getCorners <- function(dt=NULL){
  p1.la <- min(dt[,lat],na.rm=T)
  p1.lo <- max(dt[,long],na.rm=T)
  p2.la <- max(dt[,lat],na.rm=T)
  p2.lo <- min(dt[,long],na.rm=T)
  res <- data.table(c(p1.la,p2.la),c(p1.lo,p2.lo))
  setnames(res,c("lat","long"))
}
deg2rad <- function(deg) return(deg*pi/180)
#RtD (r) (* 180.0 (/ r pi)))
rad2deg <- function (rad) return(180/pi*rad)
# Great circle calculations:
gcd.dist <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

# library(doParallel)
# myCluster <- function(ncores){
#   nCores <- detectCores(all.tests = F,logical = T) -1
#   cl <- makeCluster(nCores)
#   registerDoParallel(cl)
#   cl
# #   stopCluster(cl)
# }
myTry <- function(exp){
  tryCatch({
    exp
  },error=function(e){
    print(e)
  })
}
#calcular dia de la semana (1-7) en funcion de una fecha
dayOfWeek <- function(f){ 
  r <- as.POSIXlt(f)$wday 
  r <- ifelse(r==0,7,r)
  r
}

cart.dist <- function(pts, pt) {
  n <- as.integer(length(pts[, 1]))
  dists <- vector(mode = "double", length = n)
  .C("sp_dists", 
     pts[, 1], pts[, 2], pt[1], pt[2], n, dists, 0, PACKAGE = "sp")[[6]]
}

map.dist <- function (long1, lat1, long2, lat2) 
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
isEmpty <- function(x) {
  return(length(x)==0)
}
#crea un claster de procesos mediante el paquete doSnow
myCluster <- function(ncores){
  #   t0 <- kNow();cat(match.call()[[1]],":\t")
  #   cl <- tryCatch({
  #     stopCluster(cl)
  #     cl <- makeCluster(ncores,type="SOCK")
  #   },error=function(e){
  #     cl <- makeCluster(ncores,type="SOCK")
  #     return(cl)
  #   })
  #   registerDoSNOW(cl)
  #   t1 <- kNow();print(t1-t0)
  #   cl
  myTry(registerDoMC(ncores))
}
F <- function(i){ as.Date("1970/01/01")+i }


# layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)

# # Con bokeh
# b <- gmap(lat = 41.90330, lng = -87.75789, zoom = 11, width = 700, height = 600,map_type = "roadmap") %>%
#   ly_polygons(data = dtBuildingsMap,long, lat, group=group,alpha = 0.8, col = "red") %>%
#   ly_points(data=dtCentroidBuildings,long,lat,alpha = 0.8,size=5, col = "green")
# # m %>% fitBounds(-72, 40, -70, 43)
# b

# chicago <- get_map(location = 'chicago', zoom = 11)
# p = ggmap(chicago) +
#   geom_polygon(data = data,
#                aes(x = long, y = lat, group = group),
#                fill = "#080808", color = "#080808") 
# utah@data$id = rownames(utah@data)
# ggsave(plot=p3, "foodborne_p3.png", height=5, width=5)

# for (i in nrow(shapefile)){
#   shapefile@polygons[[i]]
# }
# 
# 
# SFNeighbourhoods  = merge(sfn.f, sfn@data, by.x = 'id', by.y = 'ZIP_CODE')
# 
# 
# ## Get Chicago map
# chicagoMap <- ggmap(chicago)
# map <- chicago+ geom_polygon(data = data, aes(x = long, y = lat, group = id)) + geom_path()
# 
# 
# df=data.frame(df_deaths@coords)
# 
# sport <- readOGR(dsn = ".", "london_sport")
# 
# shapefile <- read.shapefile("chicago_city_shapefiles", "City_Boundary")
# 
# area <- readShapePoly("ne_10m_parks_and_protected_lands_area.shp")

# # Aparece la palabra roedores y ningún código sobre estado
# restNotAprroved<-foodI[Violations %like% "rodent" & !(Violations %like% "18.") & !(Violations %like% "19") & !(Violations %like% "13")]
# restLitterCommented<-foodI[Violations %like% "pest harborage" | Violations %like% "rodent nuisance"]

# # # Poligonos y edad de los edificios
# shapefile <- readOGR("Building_Footprints__current_","buildings")
# shapefile.converted <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
# 
# # # # # # Para fase de debug
# subset <- shapefile[shapefile$YEAR_BUILT==1992,]
# writeOGR(subset, ".", "buildings_subset", driver="ESRI Shapefile")
# # buildings_subset = importShapefile("buildings_subset.shp")
# shapefile <- readOGR(".","buildings_subset")
# # # # # # 
# 
# shapefile@data$id = as.character(as.numeric(rownames(shapefile@data)) + 1)
# gpclibPermit()
# dtPolyById = fortify (shapefile, region = "id")
# dtBuildingsMap = inner_join(dtPolyById, shapefile@data, by = "id") # para bokeh hace falta un df
# # Centro de masas de cada grupo coordenadas
# dtCentroidBuildings <- as.data.table(cbind(coordinates(shapefile),shapefile@data$id))
# setnames(dtCentroidBuildings,c("long","lat","id"))
# dtBuildAge <- shapefile@data[,c("YEAR_BUILT","id")]
# 
# # Centros de masas con la eddad del edificio (dt)
# dtBuildCentroidsMap <- as.data.table(merge(dtBuildAge,dtCentroidBuildings,by="id",all=F))
# # dtBuildCentroidsMap[,YEAR_BUILT_STRING:=toString(YEAR_BUILT),by="id"][,YEAR_BUILT_STRING:=NULL]
# # Cargamos mapa chicago csv
# buildAge <- as.data.table(read.csv("buildings.csv",sep=",",stringsAsFactors = F))
# buildAge2 <- buildAge[,list(ST_NAME1,YEAR_BUILT,X_COORD,Y_COORD)]
# buildAge2 <-buildAge2[,YEAR_BUILT:=as.Date(as.character(YEAR_BUILT),format="%Y")]
# # comprobación posibles repeticiones
# unique(buildAge2[,N:=.N,by=c("X_COORD","Y_COORD")])[N>1]
# #Luego se tratarán las 5 excepciones encontradas.
# l1 <- leaflet() %>% setView(lat = 41.90330, lng = -87.75789, zoom = 11) %>% 
#   addTiles(group = "OpenStreetMap") %>%
#   addPolygons(data=shapefile, weight=2) %>% 
#   addCircles(data=dtBuildCentroidsMap,lng = ~long, lat = ~lat, weight = 1,
#              radius = ~(YEAR_BUILT/120) ,col="green",popup=~toString(YEAR_BUILT), group = "Buildings Centroid") %>%
#   addCircleMarkers(data=rodent,lng=~long, lat=~lat, popup=~as.character(fecha), weight = 3, 
#                    color="#ffa500", stroke = TRUE, fillOpacity = 0.8, group = "Rodent",clusterOptions = markerClusterOptions()) %>%
#   addCircleMarkers(data=baits,lng=~long, lat=~lat, popup=~as.character(fecha), weight = 3, group="Baits",
#                    color="red", stroke = TRUE, fillOpacity = 0.8,clusterOptions = markerClusterOptions()) %>%
#   addLayersControl(baseGroups = c("OpenStreetMap"),
#                    overlayGroups = c("Buildings Centroid","Rodent","Baits"),
#                    options = layersControlOptions(collapsed = FALSE))
# l1
# # Extract values from a Raster* object at the locations of other spatial data
# # (that is, perform a spatial query). You can use coordinates (points), lines, 
# # polygons or an Extent (rectangle) object. You can also use cell numbers to extract values."
# # values <- extract(x="YourRasterLayer", y="YourSpatialPointsDataFrame")
# 
# xy <- cbind(-50, seq(-80, 80, by=20))
# extract(r, xy)