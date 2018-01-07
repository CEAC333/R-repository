
#codigo para bajar los datos de yahoo

stockData <- new.env()
Tickers   <- c("BAC","SCTY","MU","AAPL","NFLX") #nombres de los activos tal cuale estan en yahoo
FActivo  <- function(activo){
  getSymbols(activo,src = "yahoo",from = "2013-01-01",to = Sys.Date()) #periodo desde enero 2013 hasta el dia actual
  activo    <- data.frame(Cl(to.daily(get(activo))))  #daily para datos diarios
  return(activo)}
ActivosYahoo <- data.frame(
  FActivo(Tickers[1]),FActivo(Tickers[2]),FActivo(Tickers[3]),FActivo(Tickers[4]),
  FActivo(Tickers[5]))

colnames(ActivosYahoo) <- c("BAC","SCTY","MU","AAPL","NFLX") #darle nombre a la col del data frame
row.names(ActivosYahoo) <- as.Date(row.names(ActivosYahoo))+1 #darlo nombre a las filas del data frame


#codigo para sacar rendimientos logaritmicos y meterlos a otro data frame 
rendlog<-round(data.frame(diff(log(ActivosYahoo$BAC)),diff(log(ActivosYahoo$SCTY)),
                          diff(log(ActivosYahoo$MU)),diff(log(ActivosYahoo$AAPL)),
                          diff(log(ActivosYahoo$NFLX))),4)

row.names(rendlog)<-as.Date(row.names(ActivosYahoo))[-1] #nombre filas data frame
colnames(rendlog) <- c("Rend_BAC","Rend_SCTY","Rend_MU","Rend_AAPL","Rend_NFLX") #nombnre col data frame

mean_BAC<-mean(rendlog$Rend_BAC) #media de activo BAC dentro del data frame
mean_FSLR<-mean(rendlog$Rend_SCTY)
mean_MU<-mean(rendlog$Rend_MU)
mean_AAPL<-mean(rendlog$Rend_AAPL)
mean_NFLX<-mean(rendlog$Rend_NFLX)

SD_BAC<-sd(rendlog$Rend_BAC) #desviacion del actibvo BAC dentro del data frame
SD_FSLRC<-sd(rendlog$Rend_SCTY)
SD_MU<-sd(rendlog$Rend_MU)
SD_AAPL<-sd(rendlog$Rend_AAPL)
SD_NFLX<-sd(rendlog$Rend_NFLX)

estad <- data.frame(table.Stats(rendlog))#cogido que te calcula todos los estadisticos del data frame de los rendimientos