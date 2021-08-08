############# ---------- Clase 1: Calculo de VAR histórico ------------ #######
####### ----------- Limpiar ambiente ------- ###### 
rm(list = ls())
##### --------- Path: automatico  --------- #####
path <<- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = "/Ejercicio 3.+",replacement = "")
setwd(paste0(path,'/Ejercicio 3/'))
##### -------- librerias ------- ###### 
librerias <- c("readxl","ggplot2","scales",'timeSeries')



###### ----- Instalacion liberarias ------ ####3 
if(length(setdiff(librerias, rownames(installed.packages()))) > 0){
  install.packages(setdiff(librerias, rownames(installed.packages())))}
invisible(sapply(librerias, require, character.only = TRUE,quietly = TRUE))

####### --------- Paso 1: Insumos ---------- ###### 
### Defina Portafolio_acciones y asigne la informacion de Portafolio.xlsx
### Defina Historia_precios y asigne la informacion de Historia.xlsx

###### ----------- Precios acciones ---------- ##### 
Historia_precios = as.data.frame(read_excel('Historia.xlsx'))
## ----- Organice los datos 
Historia_precios = Historia_precios[-c(1,3),-c(1,2)]
## -- Ponga el nombre de las columnas -- ##
colnames(Historia_precios) = Historia_precios[1,]
#### ------ Elimine la primera fila de nombres ------ ###
Historia_precios = Historia_precios[-1,]
#### ----- Todo numerico ----- ####
Historia_precios = as.data.frame(apply(Historia_precios, 2, as.numeric))
#### ----- Arregle el formato de fechas ---- ###### 
Historia_precios$Fecha = as.Date(Historia_precios$Fecha,origin = "1899-12-30")
#### ----- Modificar el nombre de las filas con las fechas ------ ####
rownames(Historia_precios) =  Historia_precios$Fecha
#### ------- Eliminar fila de fecha ------ ####
Historia_precios = Historia_precios[,-c(1)]
Historia_precios = as.matrix(Historia_precios)
#### ---- Vector de las empresas ---- ####
Vector_de_empresas = colnames(Historia_precios)

####### -------------- Portafolio de inversion: -------------- ####### 
Portafolio_acciones = as.data.frame(read_excel('Portafolio.xlsx'))
##### ---- Se crea una columna del valor de la accion ---- ##### 
Portafolio_acciones$Valor = 0
#### ----- Se pone el nombre de las acciones a las filas ----- #### 
rownames(Portafolio_acciones) = Portafolio_acciones$Empresa
#### ----- Se elimina el vector de acciones ---- ###  
Portafolio_acciones = Portafolio_acciones[,-1]
####### --------- Fin Paso 1: Insumos ---------- ###### 

###### ----------- Paso 2: Valor del portafolio ---------- #######
### ------ Ultimo precio ------ ### 
Ultimo_precio = Historia_precios[dim(Historia_precios)[1],]
###### --------- Valor portafolio: Unidades por ultimo dato ------ #####
Portafolio_acciones$Valor = Portafolio_acciones[Vector_de_empresas,"Acciones"]*Ultimo_precio
Valor_total_portafolio = sum(Portafolio_acciones$Valor)
print(paste0('El valor total del portafolio es de $', format(Valor_total_portafolio,big.mark=",",scientific=FALSE) , ' pesos'))
###### ----------- Fin paso 2: Valor del portafolio ---------- #######

###### ---------- Paso 3: Calculo de Retornos historicos ------- ######
#### ----- Plazo de los retornos a un dia #######
Retornos_historicos = apply(Historia_precios, 2, function(x){returns(x,method = 'simple')})
##### ------ sE elimina el primero ------- #####
Retornos_historicos = Retornos_historicos[-1,]
######### ------------- Calculo de VAR ----------- #########3 
## calcular el VaR a partir de una distribucion
## 1. Retornos_historicos: Matriz de los retornos 
## 2. Confianza: Confianza de confianza para el cual se quiere el VaR. Por ejemplo 5%
### Output: vector con el VaR: en Retorno ----- ####33 
Confianza = 0.05


################# -------------- VaR consolidando acciones como un Portafolio------------- ##########3
######## -------- Portafolio consolidado -------- ##### 
Portafolio_agregado = Retornos_historicos[,Vector_de_empresas]*Ultimo_precio
###### --------- Se crea una matriz con el N de las acciones -------- ########3 
N_acciones_matrix = matrix(rep(Portafolio_acciones[Vector_de_empresas,"Acciones"],dim(Historia_precios)[1]),ncol = dim(Historia_precios)[2],byrow = T)
#####3 -------- Portafolio: Historia de los precios por la cantidad de acciones -------- ####33
Valor_portafolio = N_acciones_matrix*Historia_precios[,Vector_de_empresas]
#### ------ Suma del valor ------ #### 
Valor_portafolio = rowSums(Valor_portafolio)
##### -------- Retornos -------- ######
Retornos_valor_portafolio = returns(Valor_portafolio,method="simple")[-1]
##### ----- VaR porcentual Portafolio ------- #### 
VaR_portafolio_pct = quantile(Retornos_valor_portafolio,(Confianza))
##### -------- Calcular VaR precios: Se multiplica por el último valor del portafolio ----- #####
Valor_portafolio_final = sum(Portafolio_acciones[Vector_de_empresas,"Acciones"]*Ultimo_precio)
VaR_portafolio_niveles = VaR_portafolio_pct*Valor_portafolio_final



###### --------- Grafico --------- ########
Graficar_histograma = function(Distribucion_accion,Percentil_graficar){
  histograma = qplot(Distribucion_accion, geom = "histogram", fill = I("#ffa400"), 
                     col = I("#ffa400")) + geom_vline(aes(xintercept = as.numeric(Percentil_graficar)), size = 3,linetype = "longdash", colour = "#7b98ac") + 
    geom_vline(aes(xintercept = as.numeric(median(Distribucion_accion))), size = 3, linetype = "solid", colour = "#00577d") + 
    theme_bw() + theme(legend.background = element_rect(fill = "white", size = 2.5, linetype = "solid"), 
                       legend.title = element_text(colour = "black",size = 20, face = "bold"), legend.text = element_text(colour = "black", size = 30, face = "bold"), 
                       title = element_text(face = "bold", color = "#4e4d4d"), panel.grid.major = element_blank(), 
                       panel.border = element_blank(), plot.title = element_text(hjust = 1), 
                       axis.text = element_text(size = 20, face = "bold"), axis.title = element_text(size = 20, face = "bold"), legend.position = "top") + 
    scale_x_continuous(labels = dollar) + labs(title = "") + 
    labs(x = "Pesos") + labs(y = "\n Frecuencia \n")
  return(histograma)
}


####### ---------- Grafica histograma Agregado ----------- #######
Graficar_histograma(Distribucion_accion = Retornos_valor_portafolio*Valor_portafolio_final, Percentil_graficar = VaR_portafolio_niveles)



