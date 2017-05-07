library(jsonlite)
library(curl)
library(data.table)
## Cargamos datos del tiempo
diaIn <-as.Date("20100922",format="%Y%m%d")
wdata <- data.table(NULL) 
diasError <- c()
while (diaIn<as.Date(Sys.Date())){
  print(diaIn)
  tryCatch({
    url <- paste0("http://api.wunderground.com/api/2dd356c6f9b9d27b/history_",as.character(diaIn,format="%Y%m%d"),"/q/IL/Chicago.json")
    myJSON <- fromJSON(url)
    dailysumm <-  (myJSON$history$dailysummary)
    
    fecha <- as.data.table(dailysumm$date)
    fecha[,date:=as.Date(paste0(as.character(year),"/",as.character(mon),"/",as.character(mday)),format = "%Y/%m/%d")]
    
    dailysumm$date <- NULL
    dailysumm <- as.data.table(dailysumm)
    datosDia <- cbind(fecha[,list(date)],dailysumm)
    
    if (nrow(wdata)==0){
      wdata<- datosDia
    }else{
      wdata <- rbind(wdata,datosDia)
    }
    diaIn<-diaIn+1
  }, error = function(cond) {
    diasError <- c(diasError,diaIn)
    diaIn<-diaIn+1
    print(cond)
  })
}

write.table(wdata,file="inputs/raw/wdata.csv",sep = ";",row.names = FALSE)


wForecastData <- data.table(NULL) 
tryCatch({
  url <- paste0("http://api.wunderground.com/api/2dd356c6f9b9d27b/forecast10day/q/IL/Chicago.json")
  myJSON <- fromJSON(url)
  dailysumm <-  myJSON$forecast$simpleforecast$forecastday
  
  fecha <- as.data.table(dailysumm$date)
  fecha[,date:=as.Date(paste0(as.character(year),"/",as.character(month),"/",as.character(day)),format = "%Y/%m/%d")]
  
  dailysumm$date <- NULL
  dailysumm <- data.table(high = dailysumm$high$celsius, 
                          low = dailysumm$low$celsius,
                          pop = dailysumm$pop,
                          qpf_allday = dailysumm$qpf_allday$mm,
                          qpf_day = dailysumm$qpf_day$mm,
                          qpf_night = dailysumm$qpf_night$mm,
                          snow_allday = dailysumm$snow_allday$cm,
                          snow_day = dailysumm$snow_day$cm,
                          snow_night = dailysumm$snow_night$cm,
                          maxwind = dailysumm$maxwind$kph,
                          avewind = dailysumm$avewind$kph,
                          avehumidity = dailysumm$avehumidity,
                          maxhumidity = dailysumm$maxhumidity,
                          minhumidity = dailysumm$minhumidity)
  wForecastData <- cbind(fecha[,list(date)],dailysumm)
  
}, error = function(cond) {
  diasError <- c(diasError,as.Date(Sys.Date()))
  print(cond)
})



write.table(wForecastData,file="inputs/raw/wDataForecast.csv",sep = ";",row.names = FALSE)
