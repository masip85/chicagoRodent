# # Poligonos  de los parques
shapefile <- readOGR("inputs/raw/Parks_Aug2012/","Parks_Aug2012")
shapefile.converted <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))
shapefile <- shapefile.converted
rm(shapefile.converted)
gc()

shapefile <- shapefile[shapefile$NATURE_CEN==1 | shapefile$GARDEN==1 | 
                         shapefile$DOG_FRIEND==1 | shapefile$MOUNTAIN_B==1 |
                         shapefile$LAGOON==1 | shapefile$SLED_HILL==1 | 
                         shapefile$ZOO==1 ,]

shapefile@data$id = as.character(as.numeric(rownames(shapefile@data)) + 1)
# gpclibPermit()
# dtPolyById = fortify (shapefile, region = "id") # para bokeh
# para bokeh hace falta un df:
# dtParksMap = inner_join(dtPolyById, shapefile@data, by = "id") 
dtParkCentroids <- as.data.table(cbind(coordinates(shapefile),shapefile@data$id,
                                       shapefile@data$ACRES))
setnames(dtParkCentroids,c("long","lat","id","Acres"))
dtParkCentroids[,':='(long = as.numeric(long),
                      lat = as.numeric(lat), 
                      Acres = as.numeric(Acres))]
saveRDS(dtParkCentroids,"inputs/preProcess/parkCenters.RData")
