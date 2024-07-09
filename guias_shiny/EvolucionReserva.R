# pruebaaa
library(highcharter)

# Funciones necesarias ---------------------------------------------------------

# Progresión Geometrica 
VAn <- function(C, q, n, i, type = "immediate"){
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



# Carga de las tablas de mortalidad --------------------------------------------

probsH<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                  sheet=1)[,c(3)]))
probsM<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                  sheet=2)[,c(3)]))

ls("package:lifecontingencies")
TH <- probs2lifetable(probs=probsH, radix=100000, type="qx", name = "Mortalidad Hombres")
TM <- probs2lifetable(probs=probsM, radix=100000, type="qx", name = "Mortalidad Mujeres")


# Parámetros para el ejemplo ---------------------------------------------------

edad_inicio <- 25
edad_jubilacion <- 60
salario_ini <- 600


crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1

interes <- 6.2500 /100 #tasa actuarial

crec_salarios <- 0.02154; crec_salarios_12 <- (1+crec_salarios)^(1/12)-1 #superiodal

IVM <- 11.06/100
anio_ini <- 2010
anio_fin <- anio_ini + (edad_jubilacion-edad_inicio-1)
num_anios <- edad_jubilacion - edad_inicio

anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
num_anios_pension <- length(c((anio_fin+1):anio_muerte))


# Crecimiento de la reserva ( cotizaciones ) -----------------------------------

ev_res <- data.frame(
  anio = c(anio_ini:anio_fin),
  edad = c(edad_inicio:(edad_jubilacion-1)),
  res_acum = numeric(num_anios)
)


for (j in c(1:num_anios)) {
  
  ev_res[j,3] <-  VSn(C = (salario_ini * IVM ) * annuity(i = crec_salarios_12, n=12, type = "due"),
                      q = (1+crec_salarios) , 
                      n = j , #años de aporte hasta el momento
                      i = interes,
                      type = "due")
}

127611.9618 #considerando annuity(i = crec_salarios_12, n=12, type = "due")
10738.5100 # sin considerar annuity(i = crec_salarios_12, n=12, type = "due")


# Decrecimiento de la reserv ( pensiones ) -------------------------------------

pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]

ev_res2 <- data.frame(
  anio = c((anio_fin+1):anio_muerte),
  edad = c(edad_jubilacion:100),
  res_acum = numeric(num_anios_pension)
)


for (j in c(1:num_anios_pension)) {
  
  x=edad_jubilacion
  n=ev_res2[j,2]-edad_jubilacion+1
  m = 12
  i = crec_pensiones
  
  ev_res2[j,3] <-  ev_res[nrow(ev_res),3] - pen*12*axn(TH,x=x,n=n,i=i,payment = "due")- ((m-1)/(2*m))*(1-Exn(TH,x=x,n=n,i=i))
  
}


# Grafico evolución reserva ----------------------------------------------------

edad <- c(ev_res[,2],ev_res2[,2])
reservas <- c(ev_res[,3],ev_res2[,3])

max_res <- which(reservas==max(reservas))
last_pos <- max(which(reservas > 0))
first_neg <- min(which(reservas < 0))

highchart() %>%
  hc_chart(type = "area") %>%
  hc_title(text = "Evolución de Reservas por Edad") %>%
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
