rm(list=ls())         # Remover objetos del environment
cat("\014")           # Limpiar Consola

Pkg <- c("base","fBasics","foreach","forecast","grid","gridExtra","ggplot2","httr","lubridate",
         "moments","PerformanceAnalytics","plyr","quantmod","reshape2","RCurl","stats","scales","tseries",
         "TTR","TSA","xts","zoo")

inst <- Pkg %in% installed.packages()
if(length(Pkg[!inst]) > 0) install.packages(Pkg[!inst])
instpackages <- lapply(Pkg, library, character.only=TRUE)

options("scipen"=1000,"getSymbols.warning4.0"=FALSE,concordance=TRUE)
Sys.setlocale(category = "LC_ALL", locale = "")

Valores <- c("AC.MX","ALFAA.MX","ALPEKA.MX","ALSEA.MX","AMXL.MX","ASURB.MX","BIMBOA.MX",
             "BOLSAA.MX","CEMEXCPO.MX","COMERCIUBC.MX","ELEKTRA.MX","GAPB.MX","GENTERA.MX","GFINBURO.MX",
             "GFNORTEO.MX","GFREGIOO.MX","GMEXICOB.MX","GRUMAB.MX","GSANBORB-1.MX","ICA.MX","ICHB.MX",
             "IENOVA.MX","KIMBERA.MX","KOFL.MX","LABB.MX","LALAB.MX","LIVEPOLC-1.MX","MEXCHEM.MX",
             "NAFTRACISHRS.MX","OHLMEX.MX","PE&OLES.MX","PINFRA.MX","SANMEXB.MX","TLEVISACPO.MX","WALMEX.MX")

stockData <- new.env()
Factivo  <- function(activo){
  getSymbols(activo,src = "yahoo",from = Sys.Date()-240,to = Sys.Date())
  activo    <- Cl(get(activo))
  return(activo)}

TotalBMV <- merge(Factivo(Valores[1]),Factivo(Valores[2]),Factivo(Valores[3]),
                  Factivo(Valores[4]),Factivo(Valores[5]),Factivo(Valores[6]),Factivo(Valores[7]),
                  Factivo(Valores[8]),Factivo(Valores[9]),Factivo(Valores[10]),Factivo(Valores[11]),
                  Factivo(Valores[12]),Factivo(Valores[13]),Factivo(Valores[14]),Factivo(Valores[15]),
                  Factivo(Valores[16]),Factivo(Valores[17]),Factivo(Valores[18]),Factivo(Valores[19]),
                  Factivo(Valores[20]),Factivo(Valores[21]),Factivo(Valores[22]),Factivo(Valores[23]),
                  Factivo(Valores[24]),Factivo(Valores[25]),Factivo(Valores[26]),Factivo(Valores[27]),
                  Factivo(Valores[28]),Factivo(Valores[29]),Factivo(Valores[30]),Factivo(Valores[31]),
                  Factivo(Valores[32]),Factivo(Valores[33]),Factivo(Valores[34]),Factivo(Valores[35]))

Desv <- c()
Rendimientos <- Return.calculate(TotalBMV, method = "discrete")[-1]
Rendimientos <- Rendimientos[complete.cases(Rendimientos)]
for(i in 1:length(Valores)) Desv[i] <- round(sd(Rendimientos[i]),4)

DatosDesv <- data.frame(matrix(ncol = length(Valores), nrow = 1))
colnames(DatosDesv)  <- Valores
row.names(DatosDesv) <- "DesvEst"
DatosDesv[1,] <- Desv

Eleccion <- sort(DatosDesv*100,decreasing = TRUE)

Eleccion[1:15]

mean(Rendimientos$PINFRA.MX.Close)
