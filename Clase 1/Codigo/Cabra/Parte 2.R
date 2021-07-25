####################################################################################################
#'La idea de este script es revisar alternativas de pronostico de la trm utilizando MCO y variables 
#'de mercado. 
#'La idea es, utilizando datos de desde marzo de 2012, hacer un backtest de la proyeccion de la trm
#'para el anho 2018.
#'###################################################################################################
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### -------- librerias ------- ###### 
librerias <- c("dplyr","tidyr","xts",'ggplot2','scales','matrixStats','readxl','openxlsx','httr','lubridate','forecast')
set.seed(123)
###### ----- Instalacion paquetes ------ ####
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))


# Funciones -----------------------------------------------------------------------------------

#funcion para obtener la ibr overnight
get.IBR = function(){
  
  #' Funcion que hace webscrapping a los exceles del Banrep para obtener la ibr ON nominal
  #' OUTPUT:
  #' @return data frame con las fechas y la IBR ON
  

  #Son los links de descargade cada uno de los nodos "ON","1M", "3M", "6M".
  #Si cambian los links hay que cambiar este vector
  links = c("https://totoro.banrep.gov.co/analytics/saw.dll?Download&Format=excel2007&Extension=.xlsx&BypassCache=true&path=%2Fshared%2fSeries%20Estad%c3%adsticas_T%2F1.%20IBR%2F%201.1.IBR_Plazo%20overnight%20nominal%20para%20un%20rango%20de%20fechas%20dado%20IQY&lang=es&NQUser=publico&NQPassword=publico123&SyncOperation=1")
  
    print(paste("Procesando ON"))
    
    #se extraen los datos de la descarga
    r = GET(links,
            add_headers(
              Host="totoro.banrep.gov.co",
              `User-Agent`="Mozilla/5.0 (Windows NT 6.3; WOW64; rv:43.0) Gecko/20100101 Firefox/43.0",
              Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
              `Accept-Language` = "es-ES,es;q=0.8,en-US;q=0.5,en;q=0.3",
              `Accept-Encoding` = "gzip, deflate",
              Connection = "keep-alive"
            ))
    #se pasan a formato excel
    bin = content(r, "raw")
    archivo = "archivo.xlsx"
    writeBin(bin, archivo)
    
    #se leen
    d = try(read.xlsx(archivo, sheet = 1, detectDates = T),silent=T)
    
    if(class(d)!='try-error'){
      #SI CAMBIA EL FORMATO DE LOS EXCELES ES POSIBLE QUE HAYA QUE MODIFICAR ESTO
      #se dejan solo las fechas y la tasa nominal
      ##se busca la palabra fecha en la primera columna
      posIniFilas = grep("^fecha",tolower(d[,1]))
      ##una fila arriba de esta se busca la tasa nominal
      posCol = grep("nominal",tolower(d[posIniFilas-1,]))
      ##se saca ese pedazo del excel (la columna de fechas y la de tasa nominal sin titulos)
      d = d[(posIniFilas+1):nrow(d),c(1,posCol)]
      
      #se arregla el formato
      ##se arreglan los nombres de las columnas
      names(d) = c("Fecha", "Nominal")
      ##se cambian las comas por puntos y se quitan los % para volverlo numero
      d[,2] = gsub(x = d[,2], pattern = ",", ".")
      d[,2] = gsub(x = d[,2], pattern = "%", "")
      d[,2] = as.numeric(d[,2])
      ##se divide en 100 para que quede como una tasa normal
      d[,2] = (d[,2])/100
      ##se dejan solo los que tengan fecha y tasa
      d = d[complete.cases(d),]
      nominal = d
      #se pone el nombre de la tasa que se usa
      names(nominal)[2] = 'ON'
      
    }else{
      print(paste('No se pudo extraer IBR ON'))
      return(NULL)
    }
  
  #se quitan los nombres de las filas
  rownames(nominal) = NULL
  #se dejan solo las fechas que tienen dato para todas las tasas
  nominal = nominal[complete.cases(nominal),]
  #se convierte en fecha la variable de fecha
  nominal$Fecha = as.Date(nominal$Fecha)
  #se ordena
  nominal = nominal[order(nominal$Fecha),]
  #se anuncia cuantos datos se consiguieron
  print(paste0("Se obtuvo datos para la IBR desde ", nominal$Fecha[1], " hasta ", nominal$Fecha[nrow(nominal)] ))
  #se borra el excel que se creo
  file.remove(archivo)
  #se returna el data frame
  return(nominal)
  
}

#funcion para obtener la libor overnight
get.LIBOR = function(){
  
  #' Funcion que hace webscrapping a los exceles de la FED para obtener la libor ON
  #'
  #' OUTPUT:
  #' @return data frame con las fechas y las tasas a los plazos que se haya solicitado
  

  #se asigna el pedazo de link que cambia segun el nodo que se quiere
  appes = c("USDONTD156N")
  #se construyen los links sobre los que se hace el loop
  links = paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=2012-03-30&coed=",as.character(Sys.Date()),"&height=450&stacking=&range=Custom&mode=fred&id=",
                 appes,
                 "&transformation=lin&nd=1986-01-02&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Daily&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168")
  
  nom = 'ON'
  
  #se reporta el nodo
  print(paste("Procesando",nom))
  #se lee el csv asociado
  l = read.csv(url(links))
  #SI EL FORMATO DEL CSV CAMBIA HAY QUE CAMBIAR ESTO
  ##se pasa a formato fecha las fechas
  l[,1] = as.Date(l[,1], format ='%Y-%m-%d')
  ##se pasa a tasas
  l[,2] = suppressWarnings(as.numeric(as.character(l[,2]))/100)
  ##se pone el nombre de Fecha a la primera columna
  names(l) = c("Fecha",nom)
  libor = l

  #se quitan los nombres de las filas
  rownames(libor) = NULL
  #se organiza por fechas de menor a mayor
  libor = libor[order(libor[,1]),]
  #se dejan solo las fechas donde haya datos para todo
  libor = libor[complete.cases(libor),]
  #se reportan las fechas conseguidas
  print(paste0("Se obtuvo datos para la Libor desde ", libor$Fecha[1], " hasta ", libor$Fecha[nrow(libor)] ))
  #se retorna
  return(libor)
  
}

#funcion para obtener los precios del brent
get.Brent <- function(nom=c("Open","High", "Low","Close")){
  
  #se hace un print de que inicio el proceso
  print("Procesando Brent")
  
  r = GET("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls")
  #se pasan a formato excel
  bin <- content(r, "raw")
  archivo <- "archivo.xls"
  writeBin(bin, archivo)
  
  #se leen
  dat <- data.frame(read_excel(archivo, sheet = 2))
  
  #si cambia la estructura del archivo tendra que modificarse esto
  
  ##se quitan las filas desde donde dice Date para arriba
  posQuitar <- grep("Date",dat[,1])
  dat <- dat[-c(1:posQuitar),]
  ##la primera columna se convierte en fechas
  dat[,1] <- as.Date(as.numeric(dat[,1]), origin = "1899-12-30")
  ##se cambian los nombres de las variables
  names(dat) <- c("Fecha","Brent")
  rownames(dat) <- c()
  
  #se ordena
  dat = dat[order(dat$Fecha),]
  #se quitan los nombres de las filas
  rownames(dat) = NULL
  #se vuelven numeros las tasas
  dat[,2] = as.numeric(dat[,2])
  ##se reporta
  print(paste0("Se obtuvo datos para el Brent desde ", dat$Fecha[1], " hasta ", dat$Fecha[nrow(dat)] ))
  ##se retorna
  return(dat)
  
}

#funcion para obtener la trm
get.TRM = function(){
  
  #' Funcion que hace webscrapping a los exceles de superfin para obtener la trm
  #' diaria
  #'
  #' OUTPUT:
  #' @return data frame con las fechas y la trm
  
  #se extraen los datos de la trm de la super intendencia en formato csv
  TRM = read.csv(url("https://www.datos.gov.co/api/views/32sa-8pi3/rows.csv?accessType=DOWNLOAD"))
  
  
  #SI CAMBIA EL FORMATO DEL CSV HAY QUE CAMBIAR ESTO
  
  #se arregla el formato
  ##se dejan las columnas
  TRM = TRM[,c(3,1)]
  ##se pasa a formato fecha
  TRM[,1] = as.Date(as.character(TRM[,1]),format='%d/%m/%Y')
  ##se ordenan las fechas
  TRM = TRM[order(TRM[,1]),]
  ##se quitan los nombres de las filas y se le pone nombres a las variables
  rownames(TRM) = NULL
  names(TRM) = c("Fecha", "TRM")
  
  #se anuncia cuantos datos se consiguieron
  print(paste0("Se obtuvieron datos para la TRM desde ", TRM$Fecha[1], " hasta ", TRM$Fecha[nrow(TRM)] ))
  #se retorna
  return(TRM)
}


# Procedimiento -------------------------------------------------------------------------------


## Procesamiento de datos ----------------------------------------------------------------------

#seleccionar path
path <- gsub('Parte 2.R','',rstudioapi::getActiveDocumentContext()$path)
setwd(path)
#decidimos si hacer o no webscrapping
webscrapping <- F

if(webscrapping){
  #se utilizan las funciones de extraccion para tomar los datos que se desean
  ibr <- get.IBR()
  libor <- get.LIBOR()
  trm <- get.TRM()
  brent <- get.Brent()
  
  #se define un vector con los nombres de los objetos para manejar mas facilmente
  variables <- c('ibr','libor','trm','brent')
  
  #segun el reporte de la extraccion, la libor es la variable con menos datos, entonces con respecto a
  #esta se debe hacer la union de las demas
  
  #se quiere trabajar con datos semanales promedio, una forma de rapidamente hacer esto es utilizar
  #el paquete xts
  
  #se hace un loop para sacar los promedios semanales de cada variable y guardarlo en una lista
  promedios <- lapply(variables, function(x){
    #se trasnforma en xts la variable en cuestion
    dat <- xts(get(x)[,2],order.by = get(x)[,1])
    #se sacan los promedios semanales
    dat <- apply.weekly(dat,mean)
    #se guarda
    return(dat)
    
  })
  names(promedios) <- variables
  
  #se crea un data frame con las fechas de libor
  baseFinal <- data.frame('Fecha'=index(promedios[['libor']]),check.names = F,stringsAsFactors = F)
  
  #se hace un loop para tomar de cada variable el dato de la fecha mas cercana a las semanas de la libor
  for(i in 1:length(promedios)){
    baseFinal[,names(promedios)[i]] <- as.numeric(promedios[[i]])[sapply(baseFinal[,'Fecha'],function(x) which.min(abs(x-index(promedios[[i]]))))]
  }
  
  #se dejan los datos hasta el 2018
  baseFinal <- baseFinal[year(baseFinal[,"Fecha"])<=2018,]
  
  #write.csv(baseFinal,'./baseClase1.csv',row.names = F)
}else{
  
  baseFinal <- read.csv('./baseClase1.csv',stringsAsFactors = F)
}




## Aproximacion mercado internacional ----------------------------------------------------------

#' Bajo la intuicion de que los dolares se usan en EEUU y los pesos en Colombia, tiene sentido financiero
#' que la TRM se comporte:
#' TRM_t = TRM_(t-1)*(1+rEEUU)/(1+rCOL)
#' TRM_t/TRM_(t-1) -1 = (1+rEEUU)/(1+rCOL)-1
#' Haciendo una aproximacion de Taylor
#' retornos TRM = rEEUU - rCOL
#' Entonces, incorporando el hecho de que esta relacion no es perfecta (en particular es estocastica)
#' tiene sentido pensar en la regresion
#' retorno TRM = b0 + b1 rEEUU + b2rCol + e

#se hace una sub base con los datos de trm y las tasas
base1 <- baseFinal[,c("Fecha","ibr","trm","libor")]
#plot ibr
plot(base1[,2],type='l')
#plot trm
plot(base1[,3],type='l')
#plot retornos trm
plot(base1[-1,3]/base1[-nrow(base1),3]-1,type='l')
#plot libor
plot(base1[,4],type='l')
#se sacan aparte los datos del 2018, que son los datos donde se probara el proonostico
base1Train <- base1[year(base1[,'Fecha'])<2018,]
base1Test <- base1[year(base1[,'Fecha'])>=2018,]
#se sacan los retornos de la trm
base1Train[,'trm'] <- c(NA,base1Train[-1,"trm"]/base1Train[-nrow(base1Train),"trm"]-1)
#se hace la regresion con los datos de train ([,-1] para no tomar la fecha)
reg1 <- lm(trm~.,data=base1Train[,-1])
#proyectamos con los X's fuera de muestra
pronostico <- forecast(reg1,newdata = base1Test)
#sacamo la trm qe vamos a graficar al final
trmOrginal <- baseFinal[,"trm"]
#sacamos la trm pronosticada (pasar de retornos a niveles)
#trmProyectada <- head(base1[year(base1[,'Fecha'])<2018,'trm'],1)*cumprod(1+c(0,base1Train[-1,'trm'],pronostico$mean))
trmProyectada <- c(base1[year(base1[,'Fecha'])<2018,'trm'],
                   tail(base1[year(base1[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico$mean))
#sacamos lso intervalos de confianza
#trmProyectadaInf <- tail(base1[year(base1[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico$lower)
#trmProyectadaSup <- tail(base1[year(base1[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico$upper)
#grafico facil para revisar desempeño
plot(trmOrginal,ylim=(range(c(trmProyectada,trmOrginal))),type='l')
lines(trmProyectada,col='red')


## Incorporar mercado local --------------------------------------------------------------------

#'Aunque el modelo anterior tiene la intuicion financiera adecuada, no incorpora las "imperfecciones"
#'del mercado local. Para el caso colombiano, aunque no se transe cotidianamente con dolares, la mayor 
#'parte de la disponibilidad de esta moneda proviene de rentas petroleras. En ese orden de ideas, tiene
#'sentido pensar que los retornos de la trm tengan relacion con los del precio internacional del Brent

#se hace uso de toda la base
base2 <- baseFinal
#se pasa el brent a retornos (nunca se necesita en niveles)
base2[,'brent'] <- c(NA,base2[-1,"brent"]/base2[-nrow(base2),"brent"]-1)
#se sacan aparte los datos del 2018, que son los datos donde se probara el proonostico
base2Train <- base2[year(base1[,'Fecha'])<2018,]
base2Test <- base2[year(base1[,'Fecha'])>=2018,]
#se sacan los retornos de la trm 
base2Train[,'trm'] <- c(NA,base2Train[-1,"trm"]/base2Train[-nrow(base2Train),"trm"]-1)
#se hace la regresion con los datos de train ([,-1] para no tomar la fecha)
reg2 <- lm(trm~.,data=base2Train[,-1])
#proyectamos con los X's fuera de muestra
pronostico2 <- forecast(reg2,newdata = base2Test)
#sacamo la trm qe vamos a graficar al final
trmOrginal <- baseFinal[,"trm"]
#sacamos la trm pronosticada (pasar de retornos a niveles)
#trmProyectada1 <- head(base2[year(base2[,'Fecha'])<2018,'trm'],1)*cumprod(1+c(0,base2Train[-1,'trm'],pronostico2$mean))
trmProyectada1 <- c(base1[year(base1[,'Fecha'])<2018,'trm'],
                    tail(base1[year(base1[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico2$mean))
#sacamos lso intervalos de confianza
trmProyectadaInf <- tail(base2[year(base2[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico2$lower)
trmProyectadaSup <- tail(base2[year(base2[,'Fecha'])<2018,'trm'],1)*cumprod(1+pronostico2$upper)
#grafico facil para revisar desempeño
plot(trmOrginal,ylim=(range(c(trmProyectada1,trmOrginal))),type='l')
lines(trmProyectada1,col='red')
#agregando la proyeccion del anterior
lines(trmProyectada,col='blue')


# comparar pronosticos ------------------------------------------------------------------------

#MAE
print('Pronostico 1')
mean(abs(tail(trmOrginal-trmProyectada,52)))

print('Pronostico 2')
mean(abs(tail(trmOrginal-trmProyectada1,52)))


