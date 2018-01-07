

suppressMessages(library (fBasics))              # Para pruebas de normalidad
suppressMessages(library (forecast))             # Ajuste de modelos ARIMA
suppressMessages(library (graphics))             # Para gráfica estacional SARIMA
suppressMessages(library (grid))                 # Para dibujar flechas en las gráficas
suppressMessages(library (gridExtra))            # Extra para posicionar texto en grafica 
suppressMessages(library (ggplot2))              # Gramática de los Gráficos
suppressMessages(library (ISLR))                 # Apoyo de libro Tishirani y Hastie
suppressMessages(library (knitr))                # Docs dinámicos para R + LaTeX
suppressMessages(library (lubridate))            # Tratamiento y modificación de Fechas
suppressMessages(library (moments))              # Cálculo de momentos estadísticos
suppressMessages(library (plyr))                 # Tratamiento de bases de datos
suppressMessages(library (PerformanceAnalytics)) # Utilidades y funciones estadísticas
suppressMessages(library (quantmod))             # Descargar indicadores económicos
suppressMessages(library (robustbase))           # Estadísticos robustos básicos
suppressMessages(library (reshape2))             # Para utilizar la función MELT
suppressMessages(library (scales))               # Modificación de escala para gráficos
suppressMessages(library (tseries))              # Utilidades para series de tiempo
suppressMessages(library (xts))                  # Tratamiento de series de tiempo
suppressMessages(library (XLConnect))            # Para leer archivos de excel
suppressMessages(library (zoo))                  # Complemento de XTS

  stockData <- new.env()
  Tickers   <- c("BAC","SCTY","MU","AAPL","NFLX")
  FActivo  <- function(activo){
    getSymbols(activo,src = "yahoo",from = "2013-01-01",to = Sys.Date())
    activo    <- data.frame(Cl(to.daily(get(activo))))
    return(activo)}
  ActivosYahoo <- data.frame(
    FActivo(Tickers[1]),FActivo(Tickers[2]),FActivo(Tickers[3]),FActivo(Tickers[4]),
    FActivo(Tickers[5]))
    
  colnames(ActivosYahoo) <- c("BAC","SCTY","MU","AAPL","NFLX")
  row.names(ActivosYahoo) <- as.Date(row.names(ActivosYahoo))+1
  
  rendlog<-round(data.frame(diff(log(ActivosYahoo$BAC)),diff(log(ActivosYahoo$SCTY)),
                       diff(log(ActivosYahoo$MU)),diff(log(ActivosYahoo$AAPL)),
                       diff(log(ActivosYahoo$NFLX))),4)
  
  row.names(rendlog)<-as.Date(row.names(ActivosYahoo))[-1]
  colnames(rendlog) <- c("Rend_BAC","Rend_SCTY","Rend_MU","Rend_AAPL","Rend_NFLX")
  
  mean_BAC<-mean(rendlog$Rend_BAC)
  mean_FSLR<-mean(rendlog$Rend_SCTY)
  mean_MU<-mean(rendlog$Rend_MU)
  mean_AAPL<-mean(rendlog$Rend_AAPL)
  mean_NFLX<-mean(rendlog$Rend_NFLX)
  
  SD_BAC<-sd(rendlog$Rend_BAC)
  SD_FSLRC<-sd(rendlog$Rend_SCTY)
  SD_MU<-sd(rendlog$Rend_MU)
  SD_AAPL<-sd(rendlog$Rend_AAPL)
  SD_NFLX<-sd(rendlog$Rend_NFLX)
  
  estad <- data.frame(table.Stats(rendlog))

  
  
  