# soy ammy
library(shiny)
library(lubridate)
library(lifecontingencies)
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)
library(shinythemes)
library(shinydashboard)

options(scipen = 999)

# COEFICIENTES

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


# FUNCION PARA MINIMOS Y MAXIMOS DE PENSIONES
rango_valores_min <- list(
  list(rango = c(0, 10 * 12), valor_punto = 230),
  list(rango = c(11 * 12, 20 * 12), valor_punto = 276),
  list(rango = c(21 * 12, 30 * 12), valor_punto = 322),
  list(rango = c(31 * 12, 35 * 12), valor_punto = 368),
  list(rango = c(36 * 12, 39 * 12), valor_punto = 414),
  list(rango = c(40 * 12, Inf), valor_punto = 460)
)

rango_valores_max <- list(
  list(rango = c(0, 10 * 12), valor_punto = 1150),
  list(rango = c(15 * 12, 19 * 12), valor_punto = 1380),
  list(rango = c(20 * 12, 24 * 12), valor_punto = 1610),
  list(rango = c(25 * 12, 29 * 12), valor_punto = 1840),
  list(rango = c(30 * 12, 34 * 12), valor_punto = 2070),
  list(rango = c(35 * 12, 39 * 12), valor_punto = 2300),
  list(rango = c(4 * 120, Inf), valor_punto = 2530)
)

# Función para ajustar la pensión según mínimos
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


# Función para ajustar la pensión según máximos
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


# FUNCIONES 
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



TasaReemplazo <- function(edad, salario, anios_aporte){
  
  incremento <- 0.025339
  
  pension <- Pension(edad, salario, anios_aporte)[[1]]
  ultimo_sueldo <- salario * (1+incremento)^(anios_aporte - 1)
  
  tasa <- (pension / ultimo_sueldo) * 100
  return(tasa)
}


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

# Condiciones Mínimas 
minimo <- function(edad) {
  if (edad + 40 < 60) {
    return(edad + 40)
  } else {
    if (edad + 30 <= 64) {
      if (edad + 30 <= 60) {
        return(60)
      } else {
        return(edad + 30)
      }
    } else {
      if (edad + 15 <= 69) {
        if (edad + 15 <= 65) {
          return(65)
        } else {
          return(edad + 15)
        }
      } else {
        if (edad + 10 <= 70) {
          return(70)
        } else {
          return(edad + 10)
        }
      }
    }
  }
}

#  Se usa por defecto la siguiente información obtenida del IESS
# Aporte del afiliado (personal y patronal) al seguro= 11,06% de su salario
# Tasa de crecimiento de salarios= 2.154%


## SERVER ###
server <- function(input, output, session) {
  
  
  
  # Verificar mínimo de edad de jubilación
  
  edadmin <- reactive({
    return(minimo(input$edad_inicio))
  })
  
  output$minjub <- renderText({
    paste("La edad mínima de jubilación es:",edadmin() , "años")
  })
  
  
  
  # Ahorro
  calcularAhorro <- reactive({
    edad_inicio <- input$edad_inicio
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    edad_jubilacion <- input$edad_jubilacion
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    salario0 <- input$salario *(1+ crec_salarios)^(-input$anio_inicio + (2024 - anios_aporte))
    
    
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+crec_salarios) , 
                  n = anios_aporte, 
                  i = interes,
                  type = "due")
    return(Ahorro)
    
  })
      
    
  output$ahorro <- renderText({
      paste("El ahorro es:", round(calcularAhorro(),1))
    })
  
  output$Naportes <- renderText({
    paste("Aportó:", input$edad_jubilacion - input$edad_inicio, "años")
  })
  
  
  # Carga de las tablas de mortalidad
  
  probsH<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                    sheet=1)[,c(3)]))
  probsM<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                    sheet=2)[,c(3)]))
  
  ls("package:lifecontingencies")
  TH <- probs2lifetable(probs=probsH, radix=100000, type="qx", name = "Mortalidad Hombres")
  TM <- probs2lifetable(probs=probsM, radix=100000, type="qx", name = "Mortalidad Mujeres")
  
  



  # Cálculo del valor actual de las prestaciones a otorgarse

# Tasa de crecimiento del SBU= 2.5339%
# Tasa de crecimiento de pensiones= 1.8261%
  
  load("C:/Users/MyHP/Desktop/TIC/Shiny-TIC/obtencion_pension_prom.Rdata")
  
  # arreglo base de datos 
  #establecimiento minimo y maximo de pensiones
  
  pensiones2 <- pensiones2 %>%  mutate( prom_salario_a_usar = promedio_sueldo_real *(1.02154)^(2024 - as.numeric(format(as.Date(fecha_inicial_pension, '%Y %m %d'), "%Y"))), .before=promedio_sueldo_real)
  pensiones2 <- pensiones2 %>%  mutate(pension_final1 = pension_final *( 1.018261)^(2024 - as.numeric(format(as.Date(fecha_inicial_pension, '%Y %m %d'), "%Y"))), .before=pension_final)
  
  pensiones2 <- pensiones2 %>% mutate(pensionesfinal2 = pension_final1)
  pensiones2 <- pensiones2 %>% dplyr::filter(tipo_seguro != 'SC')
  
  pensiones2 <- pensiones2 %>%
    mutate(pensionesfinal2 = case_when(
      0 <= numero_imposiciones & numero_imposiciones <= 10 * 12 ~ ifelse(pensionesfinal2 < 230, 230, pensionesfinal2),
      11 * 12 <= numero_imposiciones & numero_imposiciones <= 20 * 12 ~ ifelse(pensionesfinal2 < 276, 276, pensionesfinal2),
      21 * 12 <= numero_imposiciones & numero_imposiciones <= 30 * 12 ~ ifelse(pensionesfinal2 < 322, 322, pensionesfinal2),
      31 * 12 <= numero_imposiciones & numero_imposiciones <= 35 * 12 ~ ifelse(pensionesfinal2 < 368, 368, pensionesfinal2),
      36 * 12 <= numero_imposiciones & numero_imposiciones <= 39 * 12 ~ ifelse(pensionesfinal2 < 414, 414, pensionesfinal2),
      40 * 12 <= numero_imposiciones ~ ifelse(pensionesfinal2 < 460, 460, pensionesfinal2),
      0 <= numero_imposiciones & numero_imposiciones <= 10 * 12 ~ ifelse(pensionesfinal2 > 1150, 1150, pensionesfinal2),
      15 * 12 <= numero_imposiciones & numero_imposiciones <= 19 * 12 ~ ifelse(pensionesfinal2 > 1380, 1380, pensionesfinal2),
      20 * 12 <= numero_imposiciones & numero_imposiciones <= 24 * 12 ~ ifelse(pensionesfinal2 > 1610, 1610, pensionesfinal2),
      25 * 12 <= numero_imposiciones & numero_imposiciones <= 29 * 12 ~ ifelse(pensionesfinal2 > 1840, 1840, pensionesfinal2),
      30 * 12 <= numero_imposiciones & numero_imposiciones <= 34 * 12 ~ ifelse(pensionesfinal2 > 2070, 2070, pensionesfinal2),
      35 * 12 <= numero_imposiciones & numero_imposiciones <= 39 * 12 ~ ifelse(pensionesfinal2 > 2300, 2300, pensionesfinal2),
      4 * 120 <= numero_imposiciones ~ ifelse(pensionesfinal2 > 2530, 2530, pensionesfinal2),
      TRUE ~ pensionesfinal2
    ))
  
  
  #Función de cálculo de la pensión promedio
  
  
  pension_promedio_fun <- function(e_j, impo, sexo1, sal_ini, anios_aporte){
    crec_salarios <- 0.02154
    
    res <- pensiones2 %>%  dplyr::filter(minimo(input$edad_inicio) <=edad_jubilacion & edad_jubilacion <= e_j + 3) %>%  
      dplyr::filter(impo + 24 >= numero_imposiciones &  numero_imposiciones >= ((minimo(input$edad_inicio) - input$edad_inicio)*12) ) %>% 
      dplyr::filter(sexo == sexo1) %>% 
      mutate(prom_salario_a_usar1 = ifelse(prom_salario_a_usar ==0, mean(pensiones2$prom_salario_a_usar[pensiones2$prom_salario_a_usar !=0]),prom_salario_a_usar)) %>% 
      dplyr::filter( Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]] - 300 <= prom_salario_a_usar1 & prom_salario_a_usar1 <= Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]]+200)
      #-300, -400
    
    prom <- mean(res$pensionesfinal2)
    
    return(prom)
  }
  
  


  calcularVApensiones <- reactive({
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    n_pensiones <- 100 - edad_jubilacion # 100 años de edad como límite
    anios_aporte <- edad_jubilacion-edad_inicio
    crec_pensiones <- 1.8261/100
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    crec_salarios <- 0.02154
    salario <- input$salario *(1+ crec_salarios)^(-input$anio_inicio + (2024 - anios_aporte))
    #Cálculo de la pensión promedio
    
    #pension_promedio <- Pension(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio))[[1]]
    pension_promedio <- pension_promedio_fun(edad_jubilacion, anios_aporte*12, sexo, salario, anios_aporte)
    
    # Calculo del VA de la pension
    i_12 <- (1+interes)^(1/12) - 1
    
    C <- pension_promedio* annuity(i = i_12, n=12, type = "due")
    if(sexo == 'M'){
      va_pension <- C * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension <- C * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    pension <- pension_promedio  
    
    return(list(va_pension, pension))
    
  })
  
  
  
  output$VApension <- renderText({
    paste("El valor actual actuarial de la pensión a otorgarse es: ", round(calcularVApensiones()[[1]],1))
  })
  
  output$cobertura <- renderText({
    paste("Porcentaje con el que debe aportar el Estado Ecuatoriano para cubrir el pago de la pensión del individuo: ", 
          round(((calcularVApensiones()[[1]]- calcularAhorro())/calcularVApensiones()[[1]])*100,1), '%')
  })
  
  output$pensionpromedio <- renderText({
    paste("La pensión promedio obtenida de la base de datos que recibirían es de: $ ", round(calcularVApensiones()[[2]],1))
  })

  output$pension_teorica_actual <- renderText({
    paste("La pensión teórica que recibiría actualmente sin las reformas es: $", 
          round(Pension(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio))[[1]], 2)
          )
  })
  
  
  output$pension_teorica_jub <- renderText({
    paste("La pensión teórica que recibiría al momento de su jubilación sin las reformas es: $", 
          round(Pension(input$edad_inicio, input$salario , (input$edad_jubilacion - input$edad_inicio))[[1]], 2)
    )
  })
  
  output$pension_teorica_jub_o <- renderText({
    paste("La pensión teórica que recibiría al momento de su jubilación capitalizando la pensión actual obtenida: $", 
          round((Pension(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio))[[1]])*(1.018261 )^((input$edad_jubilacion-input$edad_inicio)- (2024 - input$anio_inicio)),2)
    )
  })
  
  
  output$tasa_reemplazo <- renderText({
    paste("La tasa de reemplazo es: ", 
          round( TasaReemplazo(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio)), 2)
    )
  })
  
  
  
  
  
  
  # GRAFICO, CUÁNDO SE PRESENTA EL DÉFICIT DEPENDIENDO DEL APORTE DEL ESTADO DEL 10% AL 40%
  # USO DE LA INFORMACIÓN DADA POR LA BASE DE DATOS PROPORCIONADA
  
  
  
  


}









