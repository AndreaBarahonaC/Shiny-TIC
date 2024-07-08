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
library(DT)

options(scipen = 999)

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
  
  # load("C:/Users/MyHP/Desktop/TIC/Shiny-TIC/obtencion_pension_prom.Rdata") #Taynita
  load("obtencion_pension_prom.Rdata")
  
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
  
  
  # Evolucion de la reserva del individuo
  
  Reserva <- reactive({
    
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- ifelse(input$edad_jubilacion>=60,input$edad_jubilacion,60)
    n_pensiones <- 100 - edad_jubilacion # 100 años de edad como límite
    anios_aporte <- edad_jubilacion-edad_inicio
    crec_pensiones <- 1.8261/100
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    crec_salarios <- 0.02154
    salario_hoy <- input$salario *(1+ crec_salarios)^(-input$anio_inicio + (2024 - anios_aporte))
    salario_ini <- input$salario
    anio_ini <- input$anio_inicio
    
    # Reserva mientras cotiza
    
    reservas_c <- data.frame(
      edades = c(edad_inicio:(edad_jubilacion-1)),
      imposicion_anual_a_esa_edad_hoy = numeric(anios_aporte),
      reserva_a_esa_edad_hoy = numeric(anios_aporte)
    )
    
    for(i in c(0:(anios_aporte-1))){
      reservas_c$imposicion_anual_a_esa_edad_hoy[i+1] <- salario_ini*(1+0.02154)^(i)*0.1106*12*(1+interes)^(2024-anio_ini-i)
      if(i==0){
        reservas_c$reserva_a_esa_edad_hoy[i+1] <- reservas_c$imposicion_anual_a_esa_edad_hoy[i]
      }else{
        reservas_c$reserva_a_esa_edad_hoy[i+1] <- sum(reservas_c$reserva_a_esa_edad_hoy)
      }
    }
    
    # Reserva al pensionarse
    
    ahorro_hoy <- reservas_c$reserva_a_esa_edad_hoy[anios_aporte]
    pension_ini <- Pension(input$edad_inicio, input$salario , (input$edad_jubilacion - input$edad_inicio))[[1]]
    
    reservas_p <- data.frame(
      edades = c(edad_jubilacion,99),
      pension_hoy = numeric(length(c(edad_jubilacion,99))),
      gasto_anual = numeric(length(c(edad_jubilacion,99))),
      reserva_hoy = numeric(length(c(edad_jubilacion,99)))
    )
    
    for(i in c(0:length(c(edad_jubilacion,99)))){
      
      reservas_p$pension_hoy[i+1] = pension_ini*(1+0.018261)^(i)
      reservas_p$gasto_anual[i+1] = reservas_p$pension_hoy[i+1]*12
      reserva_p$reserva_hoy[i+1] = ahorro_hoy - sum(reservas_p$gasto_anual)
    }
    
    reservas_c <- reservas_c %>% mutate(res = reserva_a_esa_edad_hoy) %>% select(edades,res)
    reservas_p <- reservas_p %>% mutate(res = reserva_hoy) %>% select(edades,res)
    
    reservas <- rbind(reservas_c,reservas_p)
    
    return(list(reservas$edades,reservas$res))
    
  })
  
  
  # output$table1 <- renderTable({ Reserva() })
  
  
  
  
  
  
  # GRAFICO, CUÁNDO SE PRESENTA EL DÉFICIT DEPENDIENDO DEL APORTE DEL ESTADO DEL 10% AL 40%
  # USO DE LA INFORMACIÓN DADA POR LA BASE DE DATOS PROPORCIONADA
  
  
  
  


}









