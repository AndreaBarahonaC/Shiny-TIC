library(highcharter)
library(lubridate)
library(lifecontingencies)
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)

# Tasas ########################################################################
crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
i_actuarial <- 6.2500 /100 #tasa actuarial
crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
IVM <- 11.06/100
# Parámetros para el ejemplo ###################################################
edad_inicio <- 25
edad_jubilacion <- 60
salario_ini <- 600
anio_inicio <- 2010
anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
num_anios <- edad_jubilacion - edad_inicio

anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
num_anios_pension <- length(c((anio_fin+1):anio_muerte))



# Funciones necesarias #########################################################

VAn <- function(C, q, n, i, type = "immediate"){# Progresión Geometrica
  if(q != (1+i)){
    res <- C*(1-q^n*(1+i)^(-n))/(1+i-q)
  } else {
    res <- C*n*(1+i)^(-1)
  };
  if(type != "immediate"){
    res <- res*(1+i)
  }
  return(res)
} 
VSn <- function(C, q, n, i, type = "immediate"){
  return(VAn(C, q, n, i, type)*(1+i)^(n))
}
Coeficiente <- data.frame(An.Imposiciones = c(5:40),
                          Coef = c(0.4375, 0.4500, 0.4625,0.4750,0.4875, 0.5000, 0.5125, 0.5250, 0.5375, 0.5500, 0.5625, 0.5750, 0.5875, 0.6000, 0.6125, 0.6250, 
                                   0.6375, 0.6500, 0.6625, 0.6750, 0.6875, 0.7000, 0.7125, 0.7250, 0.7375, 0.7500, 0.7625, 0.7750, 0.7875, 0.8000, 0.8125, 0.8325,
                                   0.8605, 0.8970, 0.9430, 1.0000))
for (i in 41:100) { # Añadir nuevas filas para cuando supera los 40 años de aportes
  ultimo_coef <- tail(Coeficiente$Coef, 1)
  nuevo_coef <- ultimo_coef + 0.0125
  nueva_fila <- data.frame(An.Imposiciones = i, Coef = nuevo_coef)
  Coeficiente <- rbind(Coeficiente, nueva_fila)
}
ajustar_pension_min <- function(numero_imposiciones, pension, rango_valores) {
  for (rango_valor in rango_valores) {
    rango <- rango_valor$rango
    valor_punto <- rango_valor$valor_punto
    if (!is.na(valor_punto) && !is.na(pension) & rango[1] <= numero_imposiciones & numero_imposiciones <= rango[2]) {
      if (pension < valor_punto) {
        pension <- valor_punto
      }
    }
  }
  return(pension)
}
ajustar_pension_max <- function(numero_imposiciones, pension, rango_valores) {
  for (rango_valor in rango_valores) {
    rango <- rango_valor$rango
    valor_punto <- rango_valor$valor_punto
    if (!is.na(valor_punto) && !is.na(pension) & rango[1] <= numero_imposiciones & numero_imposiciones <= rango[2]) {
      if (pension > valor_punto) {
        pension <- valor_punto
      }
    }
  }
  return(pension)
}
rango_valores_min <- list(
  list(rango = c(0, 10 * 12), valor_punto = 230*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(11 * 12, 20 * 12), valor_punto = 276*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(21 * 12, 30 * 12), valor_punto = 322*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(31 * 12, 35 * 12), valor_punto = 368*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(36 * 12, 39 * 12), valor_punto = 414*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(40 * 12, Inf), valor_punto = 460*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024))
)
rango_valores_max <- list(
  list(rango = c(0, 10 * 12), valor_punto = 1150*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(15 * 12, 19 * 12), valor_punto = 1380*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(20 * 12, 24 * 12), valor_punto = 1610*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(25 * 12, 29 * 12), valor_punto = 1840*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(30 * 12, 34 * 12), valor_punto = 2070*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(35 * 12, 39 * 12), valor_punto = 2300*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024)),
  list(rango = c(4 * 120, Inf), valor_punto = 2530*(1+crec_SBU)^(( anio_inicio +  edad_jubilacion -  edad_inicio)- 2024))
)
Pension <- function(edad, salario, anios_aporte){
  
  incremento <- 0.025339 
  
  # Creamos un vector con los últimos 5 mejores salarios 
  mejores_5_salarios <- sapply(1:5, function(i) {
    salario * (1 + incremento)^(anios_aporte - i)
  })
  
  # pension = promedio * coef
  prom <- sum(mejores_5_salarios) / 5
  coef <- Coeficiente$Coef[Coeficiente$An.Imposiciones == anios_aporte]
  
  pension <- prom * coef 
  
  pension <- ajustar_pension_min(anios_aporte*12, pension, rango_valores_min)
  pension <- ajustar_pension_max(anios_aporte*12, pension, rango_valores_max)
  
  return(list(pension, prom))
}
axn_m <- function(TH,x,n,m,i,payment="due"){# Prima de renta actuarial fraccionada pre o pospagable 
  if(payment == "due"){
    ax<- axn(TH,x=x,n=n,i=i,payment = "due")- ((m-1)/(2*m))*(1-Exn(TH,x=x,n=n,i=i))
    return(ax)
  } else {
    ax <- axn(TH,x=x,n=n,i=i,payment = "immediate")- ((m+1)/(2*m))*(1-Exn(TH,x=x,n=n,i=i)) #PROFE: aqui en realidad es due
    return(ax)
  }
}

grafico_evolucion_reserva <- function(edad,reservas,con_o_sin){
  max_res <- which(reservas==max(reservas))
  last_pos <- max(which(reservas > 0))
  first_neg <- min(which(reservas < 0))
  highchart() %>%
    hc_chart(type = "area") %>%
    hc_title(text = "Evolución de Reservas por Edad") %>%
    hc_subtitle(text = paste(con_o_sin,"el aporte del 40% del Estado en cada pensión")) %>%
    hc_xAxis(categories = edad, title = list(text = "Edad")) %>%
    hc_yAxis(title = list(text = "Reservas")) %>%
    hc_tooltip(shared = TRUE, valueDecimals = 0) %>%
    hc_plotOptions(area = list(
      marker = list(enabled = FALSE),
      enableMouseTracking = TRUE
    )) %>%
    hc_add_series(
      name = "Aportación",
      data = reservas[1:max_res],  # datos positivos hasta 150000
      color = "blue",
      fillOpacity = 0.3
    ) %>%
    hc_add_series(
      name = "Jubilación",
      data = c(rep(NA, max_res-1), reservas[max_res:last_pos]),  # datos decreciendo de 150000 a 0
      color = "green",
      fillOpacity = 0.3
    ) %>%
    hc_add_series(
      name = "Jubilación -",
      data = c(rep(NA, last_pos-1), reservas[(last_pos):length(reservas)]),  # datos decreciendo de 0 a -150000
      color = "red",
      fillOpacity=0.3
    )
}

# Tablas de mortalidad #########################################################

probsH<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                  sheet=1)[,c(3)]))
probsM<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                  sheet=2)[,c(3)]))

ls("package:lifecontingencies")
TH <- probs2lifetable(probs=probsH, radix=100000, type="qx", name = "Mortalidad Hombres")
TM <- probs2lifetable(probs=probsM, radix=100000, type="qx", name = "Mortalidad Mujeres")



# Crecimiento de la reserva ( cotizaciones ) ###################################

ev_res <- data.frame(
  anio = c(anio_inicio:anio_fin),
  edad = c(edad_inicio:(edad_jubilacion-1)),
  res_acum = numeric(num_anios)
)

for (j in c(1:num_anios)) {
  ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = crec_SBU_12, n=12, type = "immediate"),
                      q = (1+crec_SBU) , 
                      n = j , #años de aporte hasta el momento
                      i = i_actuarial,
                      type = "due")
}

# Decrecimiento de la reserv ( pensiones ) #####################################

pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]
ahorro_total_inicial <- ev_res[nrow(ev_res),3]

################################################################################
# con 40% del aporte del estado en pensiones ###################################
################################################################################

ev_res2 <- data.frame(
  anio = c((anio_fin+1):anio_muerte),
  edad = c(edad_jubilacion:100),
  res_acum = numeric(num_anios_pension),
  gasto_anio = numeric(num_anios_pension)
)

x=edad_jubilacion
m = 12
#i = crec_pensiones
sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
ahorro_total_inicial

for (j in c(1:num_anios_pension)) {
 
  if (j==1) {
    doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
    dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
    dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
    
    ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
    ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 # Falta capitalizar el ahorro alcanzado
  }else{
    
    doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
    dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
    dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
    
    ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
    ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 
    
  }
}

# Grafico evolución reserva ----------------------------------------------------

edad <- c(ev_res[,2],ev_res2[,2])
reservas <- c(ev_res[,3],ev_res2[,3])
grafico_evolucion_reserva(edad,reservas,"Con")

################################################################################
# Sin 40% del aporte del estado en pensiones ###################################
################################################################################

ev_res2 <- data.frame(
  anio = c((anio_fin+1):anio_muerte),
  edad = c(edad_jubilacion:100),
  res_acum = numeric(num_anios_pension),
  gasto_anio = numeric(num_anios_pension)
)

x=edad_jubilacion
m = 12
i = crec_pensiones
sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
ahorro_total_inicial

for (j in c(1:num_anios_pension)) {
  
  if (j==1) {
    doce_pen <- pen*12*axn_m(TH,x,1,m,i,payment="immediate")
    dec_ter <- pen*axn(TH,x = x,n = 1,i = i,payment = "immediate")
    dec_cua <- sbu_anio_jub*axn(TH,x = x,n = 1,i = crec_SBU,payment = "immediate")
    
    ev_res2[j,"gasto_anio"] <- doce_pen + dec_ter + dec_cua 
    ev_res2[j,"res_acum"]   <- ahorro_total_inicial - ev_res2[j,"gasto_anio"]
  }else{
    
    doce_pen <- pen*12*axn_m(TH,x+j-1,1,m,i,payment="immediate")*Exn(TH,x = x,n = j-1,i = i) #fraccionada diferida
    dec_ter <- pen*axn(TH,x = x,n = 1,i = i,m = j-1, payment = "immediate") #diferida
    dec_cua <- sbu_anio_jub*axn(TH,x = x,n = 1,i = crec_SBU,m = j-1, payment = "immediate") #diferida
    
    ev_res2[j,"gasto_anio"] <- doce_pen + dec_ter + dec_cua 
    ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"] - ev_res2[j,"gasto_anio"]
    
  }
}

# Grafico evolución reserva ----------------------------------------------------

edad <- c(ev_res[,2],ev_res2[,2])
reservas <- c(ev_res[,3],ev_res2[,3])
grafico_evolucion_reserva(edad,reservas,"Sin") 
