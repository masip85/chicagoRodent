# Plot Ratas vs Antigüedad
l4 <- leaflet() %>%
  setView(lat = frameCorners[,sum(lat)/2], lng =frameCorners[,sum(long)/2], zoom = 10) %>% 
  addTiles(group = "OpenStreetMap") %>%
  addCircles(data=frameCorners,lng = ~long, lat = ~lat, weight = 1,
             radius = 1000 ,col="red") %>%
  addRasterImage(r, colors="Blues", opacity = 0.8) %>%
  addLegend(pal = colorNumeric("Blues",dtBuildSq[!is.na(square),YEAR_BUILT_Mean]), 
            values = dtBuildSq[!is.na(square),YEAR_BUILT_Mean],
            title = "Building Year") %>%
  addPolygons(data=shapefileParks, weight=1,col="Green") %>%
  addCircles(data=rodent[complete.cases(rodent)],lng = ~long, lat = ~lat, weight = 1,
             radius = ~nRats ,color="Red") # %>%  Demasiado para cpu convencional a partir de aquí
  #   addCircles(data=restaurantsSq[complete.cases(restaurantsSq)],lng = ~long, lat = ~lat, weight = 1,
  #                         radius = ~N ,color="Yellow")