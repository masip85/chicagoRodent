cargarCSVs <- function(){
  # ¿Cuanto tardan en descubrirse los problemas? : TODO
  ##### DATASET: Quejas sanidad ######
  SC <- as.data.table(
    read.csv("inputs/raw/311_Service_Requests_-_Sanitation_Code_Complaints.csv",
             sep=","))[-1]
  SC <- SC[, list(What.is.the.Nature.of.this.Code.Violation.,
                  fechaIni = as.Date(Creation.Date,format="%m/%d/%Y"),
                  fechaFin = as.Date(Completion.Date,format="%m/%d/%Y"),
                  lat=Latitude,long=Longitude,
                  xCoord=X.Coordinate,yCoord=Y.Coordinate)]
  # Problema con las basuras:
  trash <- SC[What.is.the.Nature.of.this.Code.Violation. %in% 
                c("Dumpster not being emptied", "Garbage in alley", "Garbage in yard",
                  "Overflowing carts")]
  # Agua estancada:
  waterStanding <- SC[
    What.is.the.Nature.of.this.Code.Violation. == "Standing water"]
  # Heces de perro:
  dogFeces <- SC[
    What.is.the.Nature.of.this.Code.Violation. == "Dog feces in yard"]
  
  trash[, What.is.the.Nature.of.this.Code.Violation. := NULL]
  waterStanding[, What.is.the.Nature.of.this.Code.Violation. := NULL]
  dogFeces[, What.is.the.Nature.of.this.Code.Violation. := NULL]
  
  saveRDS(trash,"inputs/preProcess/trash.RData")
  saveRDS(waterStanding,"inputs/preProcess/waterStanding.RData")
  saveRDS(dogFeces,"inputs/preProcess/dogFeces.RData")
  
  
  ##### DATASET: Quejas medioambiente ######
  EC <- as.data.table(
    read.csv("inputs/raw/CDPH_Environmental_Complaints.csv", sep=","))[-1]
  EC <- EC[ COMPLAINT.TYPE %in% c("Illegal Dumping Work Order",
                                  "ILLEGAL DUMPING WORK ORDER",
                                  "CONSTRUCTION AND DEMOLITION")]
  # Extraigo lat/long de string
  pattern <- 
    "\\(\\s{0,4}[0-9]+.[0-9]+\\s{0,3},\\s{0,4}[-][0-9]+.[0-9]+\\s{0,3}\\)"
  EC[, m := gregexpr(pattern, MAPPED.LOCATION)]
  EC <- EC[m != -1]
  EC[, latLong := regmatches(MAPPED.LOCATION, m)]
  EC[, latLong := gsub(pattern = "\\(",replacement = "", latLong)]
  EC[, latLong := gsub(pattern = "\\)",replacement = "", latLong)]
  EC[,  c("lat", "long") := (tstrsplit(latLong, ",", fixed=F))]
  
  EC <- EC[, list(COMPLAINT.DETAIL,
                  fechaIni = as.Date(COMPLAINT.DATE, format="%m/%d/%Y"),
                  fechaFin = as.Date(Modified.Date, format="%m/%d/%Y"),
                  lat = as.numeric(lat), long = as.numeric(long)
  )]
  # Elimino casos que no tienen que ver con basura:ruidos y liquidos
  EC <- EC[!grep("\\bloud\\b|\\bnoise\\b|\\boil\\b|\\bliquid\\b|\\bfluid", 
                 COMPLAINT.DETAIL, ignore.case = T )]
  
  # Obtengo escombros de obras
  constructTrash <- 
    EC[grep("workers|construct", COMPLAINT.DETAIL, ignore.case = T )]
  # Obtengo basura en general
  trashes <- 
    EC[grep("GARBAGE|TRASH|DEBRIS|JUNK", COMPLAINT.DETAIL, ignore.case = T )]
  trashes <- trashes[!(COMPLAINT.DETAIL %in% constructTrash[,COMPLAINT.DETAIL])]
  # Junto trash y trashes? : TODO
  
  # 
  trashes[, COMPLAINT.DETAIL := NULL]
  constructTrash[, COMPLAINT.DETAIL := NULL]
  saveRDS(trashes,"inputs/preProcess/trashes.RData")
  saveRDS(constructTrash,"inputs/preProcess/constructTrash.RData")
  
  
  ##### DATASET: Datos del tiempo ######
  # Cargamos datos tiempo
  wdata <- fread("inputs/raw/wdata.csv", sep = ";", 
                 encoding = "UTF-8", verbose = verbose)
  wdata <- wdata[, list(date, 
                        maxtempm, mintempm, 
                        maxwspdm, meanwindspdm, 
                        precipm, snowdepthm, 
                        maxhumidity,minhumidity)]
  wdata[, ':='(fecha=as.Date(date), 
               maxtempm = as.integer(maxtempm), mintempm = as.integer(mintempm), 
               maxwspdm = as.integer(maxwspdm), meanwindspdm = as.integer(meanwindspdm), 
               precipm = as.numeric(precipm), snowdepthm = as.numeric(snowdepthm),
               maxhumidity = as.integer(maxhumidity),minhumidity = as.integer(minhumidity) )]
  wdata[,date := NULL]
  
  wFdata <- fread("inputs/raw/wDataForecast.csv", sep = ";", 
                  encoding = "UTF-8", verbose = verbose)
  wFdata <- wFdata[, list(high = as.integer(high), low = as.integer(low), 
                          maxwind, avewind, 
                          qpf_allday = as.numeric(qpf_allday), 
                          snow_allday = as.numeric(snow_allday), 
                          maxhumidity,minhumidity,
                          fecha = as.Date(date))]
  setnames(wFdata, names(wdata))
  wdata <- rbind(wdata,wFdata)
  
  saveRDS(wdata,"inputs/preProcess/weather.RData")
  
  ##### DATASET: Inspecciones sanidad restaurantes ######
  # Cargamos datos foodInspections
  foodI <- as.data.table(read.csv("inputs/raw/Food_Inspections.csv", sep = ","))
  setnames(foodI, make.names(colnames(foodI))) 
  ViolationsSplitted <- strsplit(foodI[,Violations],split="\\|")
  foodI <- foodI[,list(fechaIni=as.Date(Inspection.Date,format="%m/%d/%Y"),
                       lat=Latitude,long=Longitude)]
  
  for (idx in 1:length(ViolationsSplitted)){
    #18. NO EVIDENCE OF RODENT OR INSECT OUTER OPENINGS PROTECTED/RODENT PROOFED
    # Aparece evidencia de roedor
    if (length(grep("18. NO EVIDENCE.*",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=1,rodent2=0,rodent3=0)]
    }
    # Aparece evidencia de roedor corregida
    if (length(grep("18. NO EVIDENCE.*CORRECT|18. NO EVIDENCE.*ABATED",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=0,rodent2=0,rodent3=0)]
    }
    #13. NO EVIDENCE OF RODENT OR INSECT INFESTATION, NO BIRDS, TURTLES OR OTHER ANIMALS
    if (length(grep("13. NO EVIDENCE.*",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=0,rodent2=1,rodent3=0)]
    }
    # Aparece evidencia de roedor corregida
    if (length(grep("13. NO EVIDENCE.*CORRECT|13. NO EVIDENCE.*ABATED",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=0,rodent2=0,rodent3=0)]
    }
    if (length(grep("19. OUTSIDE GARBAGE",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=0,rodent2=0,rodent3=1)]
    }
    # Aparece evidencia de roedor corregida
    if (length(grep("19. OUTSIDE GARBAGE.*CORRECT|19. OUTSIDE GARBAGE.*ABATED",
                    ViolationsSplitted[[idx]],ignore.case = T,value=T))!=0){
      foodI[idx,':='(rodent1=0,rodent2=0,rodent3=0)]
    }
  }
  
  foodI <- foodI[!is.na(rodent1) & !is.na(rodent2) & !is.na(rodent3)] 
  
  # foodI <- foodI[,list(fechaIni=as.Date(Inspection.Date,format="%m/%d/%Y"),lat=Latitude,long=Longitude,nivel=Rodent)]
  setkey(foodI,NULL)
  foodI <- unique(foodI) # elimino valores repetidos
  # foodI <- foodI[,fechaFin:= IntToDate(kLag2(foodI,"fecha",c("lat","long"),1))]
  # foodI[,monthIni:=format(fechaIni,"%m")]
  # foodI[,monthFin:=format(fechaFin,"%m")]
  saveRDS(foodI,"inputs/preProcess/foodI.RData")
  
  ##### DATASET: Licencias de negocios ######
  # Cargamos datos de licencias de restaurantes.
  restaurants <- as.data.table(read.csv("inputs/raw/Business_Licenses.csv",sep=","))
  # restaurants2 <- fread("inputs/raw/Business_Licenses.csv",sep=",",strip.white = FALSE,na.strings = NULL,nrow = 100)
  # setnames(restaurants2, make.names(colnames(restaurants2))) 
  restaurants <- restaurants[LICENSE.DESCRIPTION %in% 
                               c("Retail Food Establishment", 
                                 "Retail Food Est.-Supplemental License for Dog-Friendly Areas", 
                                 "Food - Shared Kitchen", 
                                 "Food - Shared Kitchen Short-Term User", 
                                 "Retail Food - Seasonal Lakefront Food Establishment", 
                                 "Special Event Food", 
                                 "Food - Shared Kitchen - Supplemental")]
  restaurants <- restaurants[,list(LICENSE.STATUS,LICENSE.TERM.START.DATE,
                                   LICENSE.TERM.EXPIRATION.DATE,LATITUDE,
                                   LONGITUDE),by="LOCATION"]
  restaurants <- restaurants[LICENSE.STATUS=="AAI"]
  restaurants <- restaurants[,list(fechaIni=as.Date(LICENSE.TERM.START.DATE,
                                                    format="%m/%d/%Y"),
                                   fechaFin=as.Date(LICENSE.TERM.EXPIRATION.DATE,
                                                    format="%m/%d/%Y"),
                                   lat=LATITUDE,long=LONGITUDE)]
  setkey(restaurants,NULL)
  restaurants <- unique(restaurants)
  # restaurants[,monthIni:=format(fechaIni,"%m")]
  # restaurants[,monthFin:=format(fechaFin,"%m")]
  saveRDS(restaurants,"inputs/preProcess/restaurants.RData")
  
  ##### DATASET: Acciones Roedores y puesta de cebos ######
  # Cargamos datos RodentBaiting
  rodentBaiting <- as.data.table(
    read.csv("inputs/raw/311_Service_Requests_-_Rodent_Baiting.csv",sep=","))
  rodentBaiting <- rodentBaiting[!is.na(Creation.Date) & !is.na(Location) &
                                   Status=="Completed"]
  # QUITO LAS FALSAS ALARMAS
  rodentBaiting <- rodentBaiting[
    Most.Recent.Action!="Area inspected, no cause and no baiting"]
  
  # Si dice Inspected and Baited, pero numero de cebos o de ratas sale 0,
  # como es un contrasentido,lo almaceno a parte, por lo que pudiera significar
  IB <- rodentBaiting[(Number.of.Premises.with.Rats==0 | 
                         is.na(Number.of.Premises.with.Rats)) &
                        (Number.of.Premises.Baited==0 |
                           is.na(Number.of.Premises.Baited))  & 
                        Most.Recent.Action != "Area inspected, no cause and no baiting",
                      list(Creation.Date,X.Coordinate,Y.Coordinate,
                           Latitude,Longitude,Number.of.Premises.with.Rats)]
  saveRDS(IB,"inputs/preProcess/IB_0Rats.RData")
  
  # Numero Ratas     
  rodent <- rodentBaiting[(Number.of.Premises.with.Rats!=0 & 
                             !is.na(Number.of.Premises.with.Rats)),
                          list(fecha=as.Date(Creation.Date,format="%m/%d/%Y"),
                               lat=Latitude,long=Longitude,
                               xCoord=X.Coordinate,yCoord=Y.Coordinate,
                               nRats=Number.of.Premises.with.Rats)]
  setkey(rodent)
  rodent <- unique(rodent)
  rodent <- rodent[order(-fecha)]
  # rodent[,fechaFin:=shift(fechaIni,n = 1),by=c("lat","long")]
  saveRDS(rodent,"inputs/preProcess/rodent.RData")
  
  # Numero Cebos
  baits <- rodentBaiting[Number.of.Premises.Baited!=0,
                         list(fecha=as.Date(Creation.Date,format="%m/%d/%Y"),
                              lat=Latitude,long=Longitude,
                              xCoord=X.Coordinate,yCoord=Y.Coordinate,
                              baits =Number.of.Premises.Baited)]
  setkey(baits)
  baits <- unique(baits)
  baits <- baits[order(-fecha)]
  saveRDS(baits,"inputs/preProcess/baits.RData")
  
  ##### DATASET: Quejas edificios abandonados ######
  # Cargamos datos abandoned building
  abandonedBuild <-
    as.data.table(read.csv("inputs/raw/311_Service_Requests_-_Vacant_and_Abandoned_Buildings_Reported.csv",
                           sep=","))
  # Casos poteciales de rodent
  abandonedBuildRodent <- 
    abandonedBuild[grep("rodent", IF.THE.BUILDING.IS.OPEN..WHERE.IS.THE.ENTRY.POINT.,
                        ignore.case = T)]
  abandonedBuildRodent <- 
    abandonedBuildRodent[,list(fecha=as.Date(DATE.SERVICE.REQUEST.WAS.RECEIVED,
                                             format="%m/%d/%Y"),
                               lat=LATITUDE,long=LONGITUDE,xCoord=X.COORDINATE,
                               yCoord = Y.COORDINATE)]
  
  # Fugas de agua en edificios abandonados
  waterLeak <- abandonedBuild[grep("water", 
                                   IF.THE.BUILDING.IS.OPEN..WHERE.IS.THE.ENTRY.POINT.,
                                   ignore.case = T)]
  waterLeak <- waterLeak[grep("run|pipe|flood|leak", 
                              IF.THE.BUILDING.IS.OPEN..WHERE.IS.THE.ENTRY.POINT.,
                              ignore.case = T)]
  waterLeak <- waterLeak[,list(fecha=as.Date(DATE.SERVICE.REQUEST.WAS.RECEIVED,
                                             format="%m/%d/%Y"),
                               lat=LATITUDE,long=LONGITUDE,xCoord=X.COORDINATE,
                               yCoord = Y.COORDINATE)]
  
  # Listas de edificios abandonados
  abandonedBuild <- abandonedBuild[,list(lat=LATITUDE,long=LONGITUDE,
                                         xCoord=X.COORDINATE,
                                         yCoord=Y.COORDINATE)] 
  setkey(abandonedBuild,NULL)
  abandonedBuild <- unique(abandonedBuild)
  # abandonedBuildings <- abandonedBuildings[,.N,by=c("LATITUDE","LONGITUDE")]
  # abandonedBuildings[N==1]
  saveRDS(abandonedBuild,"inputs/preProcess/abandonedBuild.RData")
  saveRDS(waterLeak,"inputs/preProcess/waterLeak.RData")
  saveRDS(abandonedBuildRodent,"inputs/preProcess/abandonedBuildRodent.RData")
  
  list(wdata,foodI,restaurants,rodent,baits,abandonedBuild,abandonedBuildRodent,
       trash,waterStanding,dogFeces,trashes,constructTrash)
}