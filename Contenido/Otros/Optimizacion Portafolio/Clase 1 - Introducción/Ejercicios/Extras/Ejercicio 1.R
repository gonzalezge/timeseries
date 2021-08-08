#### --- Información del Bono --- ####
# Ticker	TFIT16240724
# Fecha de emisión: 2008-07-24
# Fecha de vencimiento:2024-07-24
# Precio sucio	122.1
# Nocional = 100
# Tasa Cupón: 10%
# Pago de Cupon anual: cada 07-24
# Ultimo Pago Cupón 	2019-07-24
# Fecha de valoracion 2019-10-17
#### ---------------------------- ####
##### ---------- Parametros -------- ######### 

Tasa = 5.196/100
Ultimo_pago_cupon= '2019-07-24'
Fecha_vencimiento = '2024-07-24'
Fecha_valoracion = Sys.Date()
Cupon = 0.10
Nocional = 100
#### -----------------------------#### 
### ---- Paso 1: Calcular flujos futuros ------ ### 
### ---- ¿Cuántos cupones le quedan a este Bono? ---- ### 

Fechas_cupones = seq(from=as.Date(Ultimo_pago_cupon),to = as.Date(Fecha_vencimiento),by = 'year')
### ---- Se quita el primero, por que ya se pagó ----- ###
Fechas_cupones = Fechas_cupones[-1]
### --- Tamaño del vector: indica los cupones que quedan por pagar ---- ### 
N_cupones = length(Fechas_cupones)
print(paste0('A este bono aún le quedan ',N_cupones,' cupones por pagar.'))
### ----- Construir Estructura ------ #### 
Estructura_flujos = data.frame(Fecha_cupon = Fechas_cupones,Dias = 0,Year=0,Flujos = 0, VP_flujos=0, VP_T = 0)
### ---- Diferencia de días entre los pagos de los cupones y La fecha de Valoración --- ### 
Fecha_valoracion = as.Date(Fecha_valoracion)
Estructura_flujos$Dias = sapply(Estructura_flujos[,"Fecha_cupon"], function(x){x-Fecha_valoracion})
### --- Calcular la fecha de los dias en años 365  --- ###
Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})

### --- Calcular pago cupones --- ### 
for (i in c(1:N_cupones)) {
  print(paste0('Cupón ',i))
  ### ---- Cupón antes del vencimiento ---- ###
  if (i < N_cupones) {
    Estructura_flujos[i,'Flujos'] = Cupon*Nocional
    
    ### ---- En el vencimiento: Cupón más Nocional ---- ###
  } else if (i == N_cupones) {
    Estructura_flujos[i,'Flujos'] = Nocional*(1+Cupon) 
  }
  
}

### ---- Calcular valor de los flujos ---- ###
for(j in c(1:N_cupones)){
  Estructura_flujos[j,'VP_flujos'] = Estructura_flujos[j,"Flujos"]/(1+Tasa)^Estructura_flujos[j,'Year']
  
}

### --- Calcular la fecha de los dias en años 365  --- ###
Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})

### --- Para calcular la duración se necesita el VP flujos * el Tiempo ----- ####### 
Estructura_flujos[,"VP_T"] = Estructura_flujos[,"Year"]*Estructura_flujos[,"VP_flujos"]
### --- El precio sucio es la suma de los flujos en valor presente ---- ### 
Precio_sucio = sum(Estructura_flujos[,"VP_flujos"]) 
### --- Precio limpio --- ### 
### --- Calcular cupón corrido ---- ###
Cupon_corrido = (as.numeric(as.Date(Fecha_valoracion)-as.Date(Ultimo_pago_cupon))/365)*(Cupon*Nocional)
### --- Precio limpio: Precio sucio - Cupon corrido --- ###
Precio_limpio = Precio_sucio-Cupon_corrido
### --- Duración --- ###
#### ---- La suma de los flujos en valor presente * T sobre precio sucio ---- ###
Duracion = sum(Estructura_flujos[,"VP_T"])/Precio_sucio 
### --- Duración ajustada: Duracion --- ### 
Duracion_ajustada = Duracion/(1+Tasa)
#### ------- Consolidacion de resultados ------ ########
Resultados = data.frame('Precio_sucio' = Precio_sucio,'Precio_limpio'=Precio_limpio, 'Cupon_corrido' = Cupon_corrido, 'Duracion' = Duracion, 'Duracion_ajustada' = Duracion_ajustada)






###### -------- Etapa 2: Funcion de valorar ----- ##### 
### --- Definir una funcion ---- #### 
Valorar_bono = function(Tasa, Ultimo_pago_cupon,Fecha_vencimiento, Cupon,Nocional,Fecha_valoracion){
  #### -----------------------------#### 
  ### ---- Paso 1: Calcular flujos futuros ------ ### 
  ### ---- ¿Cuántos cupones le quedan a este Bono? ---- ### 
  Fechas_cupones = seq(from=as.Date(Ultimo_pago_cupon),to = as.Date(Fecha_vencimiento),by = 'year')
  ### ---- Se quita el primero, por que ya se pagó ----- ###
  Fechas_cupones = Fechas_cupones[-1]
  ### --- Tamaño del vector: indica los cupones que quedan por pagar ---- ### 
  N_cupones = length(Fechas_cupones)
  print(paste0('A este bono aún le quedan ',N_cupones,' cupones por pagar.'))
  ### ----- Construir Estructura ------ #### 
  Estructura_flujos = data.frame(Fecha_cupon = Fechas_cupones,Dias = 0,Year=0,Flujos = 0, VP_flujos=0, VP_T = 0)
  ### ---- Diferencia de días entre los pagos de los cupones y La fecha de Valoración --- ### 
  Fecha_valoracion = as.Date(Fecha_valoracion)
  Estructura_flujos$Dias = sapply(Estructura_flujos[,"Fecha_cupon"], function(x){x-Fecha_valoracion})
  ### --- Calcular la fecha de los dias en años 365  --- ###
  Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})
  
  ### --- Calcular pago cupones --- ### 
  for (i in c(1:N_cupones)) {
    print(paste0('Cupón ',i))
    ### ---- Cupón antes del vencimiento ---- ###
    if (i < N_cupones) {
      Estructura_flujos[i,'Flujos'] = Cupon*Nocional
      
      ### ---- En el vencimiento: Cupón más Nocional ---- ###
    } else if (i == N_cupones) {
      Estructura_flujos[i,'Flujos'] = Nocional*(1+Cupon) 
    }
    
  }
  ### ---- Calcular valor de los flujos ---- ###
  for(j in c(1:N_cupones)){
    Estructura_flujos[j,'VP_flujos'] = Estructura_flujos[j,"Flujos"]/(1+Tasa)^Estructura_flujos[j,'Year']
    
  }
  
  ### --- Calcular la fecha de los dias en años 365  --- ###
  Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})
  
  ### --- Para calcular la duración se necesita el VP flujos * el Tiempo ----- ####### 
  Estructura_flujos[,"VP_T"] = Estructura_flujos[,"Year"]*Estructura_flujos[,"VP_flujos"]
  ### --- El precio sucio es la suma de los flujos en valor presente ---- ### 
  Precio_sucio = sum(Estructura_flujos[,"VP_flujos"]) 
  ### --- Precio limpio --- ### 
  ### --- Calcular cupón corrido ---- ###
  Cupon_corrido = (as.numeric(as.Date(Fecha_valoracion)-as.Date(Ultimo_pago_cupon))/365)*(Cupon*Nocional)
  ### --- Precio limpio: Precio sucio - Cupon corrido --- ###
  Precio_limpio = Precio_sucio-Cupon_corrido
  ### --- Duración --- ###
  #### ---- La suma de los flujos en valor presente * T sobre precio sucio ---- ###
  Duracion = sum(Estructura_flujos[,"VP_T"])/Precio_sucio 
  ### --- Duración ajustada: Duracion --- ### 
  Duracion_ajustada = Duracion/(1+Tasa)
  #### ------- Consolidacion de resultados ------ ########
  Resultados = data.frame('Precio_sucio' = Precio_sucio,'Precio_limpio'=Precio_limpio, 'Cupon_corrido' = Cupon_corrido, 'Duracion' = Duracion, 'Duracion_ajustada' = Duracion_ajustada)
  return(Resultados)
}


Valorar_bono(Tasa = 5.196/100,
             Ultimo_pago_cupon = '2019-07-24',
             Fecha_vencimiento = '2024-07-24',
             Fecha_valoracion =  Sys.Date(),
             Cupon = 0.10,
             Nocional = 100)


#### --- Probemos la función con otro Bono ---- ####
#### --- Información del Bono --- ####
# Ticker	TFIT16280428
# Fecha de emisión: 2012-04-28
# Fecha de vencimiento:2028-04-28
# Precio sucio	103.372
# Nocional = 100
# Tasa Cupón: 6%
# Pago de Cupon anual: cada 07-24
# Ultimo Pago Cupón 	2019-04-28
# Fecha de valoracion 2019-10-17
# Tasa 5.91% 
Valorar_bono(Tasa = 5.91/100,
             Ultimo_pago_cupon = '2019-04-28',
             Fecha_vencimiento = '2028-04-28',
             Fecha_valoracion =  Sys.Date(),
             Cupon = 0.06,
             Nocional = 100)


### ---- Etapa 3: Encontrar TIR ---- #### 
### --- Vamos a editar nuestra funcion anterior para que solo devuelva el precio sucio --- ###
###### -------- Etapa 2: Funcion de valorar ----- ##### 
### --- Definir una funcion ---- #### 
Valorar_bono_sucio = function(Precio_sucio_observado,Tasa, Ultimo_pago_cupon,Fecha_vencimiento, Cupon,Nocional,Fecha_valoracion){
  #### -----------------------------#### 
  ### ---- Paso 1: Calcular flujos futuros ------ ### 
  ### ---- ¿Cuántos cupones le quedan a este Bono? ---- ### 
  Fechas_cupones = seq(from=as.Date(Ultimo_pago_cupon),to = as.Date(Fecha_vencimiento),by = 'year')
  ### ---- Se quita el primero, por que ya se pagó ----- ###
  Fechas_cupones = Fechas_cupones[-1]
  ### --- Tamaño del vector: indica los cupones que quedan por pagar ---- ### 
  N_cupones = length(Fechas_cupones)
  print(paste0('A este bono aún le quedan ',N_cupones,' cupones por pagar.'))
  ### ----- Construir Estructura ------ #### 
  Estructura_flujos = data.frame(Fecha_cupon = Fechas_cupones,Dias = 0,Year=0,Flujos = 0, VP_flujos=0, VP_T = 0)
  ### ---- Diferencia de días entre los pagos de los cupones y La fecha de Valoración --- ### 
  Fecha_valoracion = as.Date(Fecha_valoracion)
  Estructura_flujos$Dias = sapply(Estructura_flujos[,"Fecha_cupon"], function(x){x-Fecha_valoracion})
  ### --- Calcular la fecha de los dias en años 365  --- ###
  Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})
  
  ### --- Calcular pago cupones --- ### 
  for (i in c(1:N_cupones)) {
    print(paste0('Cupón ',i))
    ### ---- Cupón antes del vencimiento ---- ###
    if (i < N_cupones) {
      Estructura_flujos[i,'Flujos'] = Cupon*Nocional
      
      ### ---- En el vencimiento: Cupón más Nocional ---- ###
    } else if (i == N_cupones) {
      Estructura_flujos[i,'Flujos'] = Nocional*(1+Cupon) 
    }
    
  }
  ### ---- Calcular valor de los flujos ---- ###
  for(j in c(1:N_cupones)){
    Estructura_flujos[j,'VP_flujos'] = Estructura_flujos[j,"Flujos"]/(1+Tasa)^Estructura_flujos[j,'Year']
    
  }
  
  ### --- Calcular la fecha de los dias en años 365  --- ###
  Estructura_flujos$Year= sapply(Estructura_flujos[,"Dias"], function(x){x/365})
  
  ### --- Para calcular la duración se necesita el VP flujos * el Tiempo ----- ####### 
  Estructura_flujos[,"VP_T"] = Estructura_flujos[,"Year"]*Estructura_flujos[,"VP_flujos"]
  ### --- El precio sucio es la suma de los flujos en valor presente ---- ### 
  Precio_sucio = sum(Estructura_flujos[,"VP_flujos"]) 
  ### --- Precio limpio --- ### 
  ### --- Calcular cupón corrido ---- ###
  Cupon_corrido = (as.numeric(as.Date(Fecha_valoracion)-as.Date(Ultimo_pago_cupon))/365)*(Cupon*Nocional)
  ### --- Precio limpio: Precio sucio - Cupon corrido --- ###
  Precio_limpio = Precio_sucio-Cupon_corrido
  ### --- Duración --- ###
  #### ---- La suma de los flujos en valor presente * T sobre precio sucio ---- ###
  Duracion = sum(Estructura_flujos[,"VP_T"])/Precio_sucio 
  ### --- Duración ajustada: Duracion --- ### 
  Duracion_ajustada = Duracion/(1+Tasa)
  #### ------- Consolidacion de resultados ------ ########
  return(Precio_sucio-Precio_sucio_observado)
}



Resultado = Valorar_bono_sucio(Precio_sucio_observado = 122.0898,Tasa = 5.196/100,
             Ultimo_pago_cupon = '2019-07-24',
             Fecha_vencimiento = '2024-07-24',
             Fecha_valoracion =  Sys.Date(),
             Cupon = 0.10,
             Nocional = 100)

 
Resultado_optimizador = uniroot(f = Valorar_bono_sucio,
        Precio_sucio_observado = 122.0898,
        Ultimo_pago_cupon = '2019-07-24',
        Fecha_vencimiento = '2024-07-24',
        Fecha_valoracion =  Sys.Date(),
        Cupon = 0.10,
        Nocional = 100,interval = c(0,1))

### --- La TIR es --- ### 
TIR = Resultado_optimizador$root*100
