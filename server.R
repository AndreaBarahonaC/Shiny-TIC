suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(lifecontingencies)))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(shinythemes)))
suppressMessages(suppressWarnings(library(shinydashboard)))
suppressMessages(suppressWarnings(library(highcharter)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(kableExtra)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(reactable)))
suppressMessages(suppressWarnings(library(shinyvalidate)))

options(scipen = 999)



## SERVER #################
server <- function(input, output, session) {
  # Funciones necesarias ----
  Pension <- function(edad, salario, anios_aporte){
    
    if(salario < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    # Creamos un vector con los últimos 5 mejores salarios 
    mejores_5_salarios <- sapply(1:5, function(i) {
      salario * (1 + incremento)^(anios_aporte - i)
    })
    
    # pension = promedio * coef
    prom <- sum(mejores_5_salarios) / 5
    coef <- Coeficiente$Coef[Coeficiente$An.Imposiciones == anios_aporte]
    
    pension <- prom * coef 
    
    pension <- ajustar_pension_min(anios_aporte*12, pension, rango_valores_min())
    pension <- ajustar_pension_max(anios_aporte*12, pension, rango_valores_max())
    
    return(list(pension, prom))
  }
  PensionActual <- function(edad, salario, anios_aporte){
    
    if(salario < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    } 
    
    # Creamos un vector con los últimos 5 mejores salarios 
    mejores_5_salarios <- sapply(1:5, function(i) {
      salario * (1 + incremento)^(anios_aporte - i)
    })
    
    # pension = promedio * coef
    prom <- sum(mejores_5_salarios) / 5
    coef <- Coeficiente$Coef[Coeficiente$An.Imposiciones == anios_aporte]
    
    pension <- prom * coef 
    
    pension <- ajustar_pension_min(anios_aporte*12, pension, rango_valores_min_A())
    pension <- ajustar_pension_max(anios_aporte*12, pension, rango_valores_max_A())
    
    return(list(pension, prom))
  }
  TasaReemplazo <- function(edad, salario, anios_aporte){
    
    if(salario < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    pension <- Pension(edad, salario, anios_aporte)[[1]]
    ultimo_sueldo <- salario * (1+incremento)^(anios_aporte - 1)
    
    tasa <- (pension / ultimo_sueldo) * 100
    
    return(tasa)
  }
  
  # Funciones para minimos y máximos de pensiones ----
  rango_valores_min <- reactive({
    rango_valores_min <- list(
      list(rango = c(0, 10 * 12), valor_punto = 230*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(11 * 12, 20 * 12), valor_punto = 276*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(21 * 12, 30 * 12), valor_punto = 322*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(31 * 12, 35 * 12), valor_punto = 368*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(36 * 12, 39 * 12), valor_punto = 414*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(40 * 12, Inf), valor_punto = 460*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1))
    )
    return(rango_valores_min)
  })
  rango_valores_max <- reactive({
    rango_valores_max <- list(
      list(rango = c(0, 10 * 12), valor_punto = 1150*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(15 * 12, 19 * 12), valor_punto = 1380*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(20 * 12, 24 * 12), valor_punto = 1610*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(25 * 12, 29 * 12), valor_punto = 1840*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(30 * 12, 34 * 12), valor_punto = 2070*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(35 * 12, 39 * 12), valor_punto = 2300*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1)),
      list(rango = c(4 * 120, Inf), valor_punto = 2530*(1+crec_SBU)^((input$anio_inicio + input$edad_jubilacion - input$edad_inicio)- 2024-1))
    )
    return(rango_valores_max)
  })
  rango_valores_min_A <- reactive({
    rango_valores_min <- list(
      list(rango = c(0, 10 * 12), valor_punto = 230),
      list(rango = c(11 * 12, 20 * 12), valor_punto = 276),
      list(rango = c(21 * 12, 30 * 12), valor_punto = 322),
      list(rango = c(31 * 12, 35 * 12), valor_punto = 368),
      list(rango = c(36 * 12, 39 * 12), valor_punto = 414),
      list(rango = c(40 * 12, Inf), valor_punto = 460)
    )
    return(rango_valores_min)
  })
  rango_valores_max_A <- reactive({
    rango_valores_max <- list(
      list(rango = c(0, 10 * 12), valor_punto = 1150),
      list(rango = c(15 * 12, 19 * 12), valor_punto = 1380),
      list(rango = c(20 * 12, 24 * 12), valor_punto = 1610),
      list(rango = c(25 * 12, 29 * 12), valor_punto = 1840),
      list(rango = c(30 * 12, 34 * 12), valor_punto = 2070),
      list(rango = c(35 * 12, 39 * 12), valor_punto = 2300),
      list(rango = c(4 * 120, Inf), valor_punto = 2530)
    )
    return(rango_valores_max)
  })
  
  # Verificar mínimo de edad de jubilación -----
  edadmin <- reactive({
    return(minimo(input$edad_inicio))
  })
  output$minjub <- renderText({
    paste("La edad mínima de jubilación es:",edadmin() , "años")
  })
  
  # Ahorro ----
  calcularAhorroTotal<- reactive({
    edad_inicio <- input$edad_inicio
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    edad_jubilacion <- input$edad_jubilacion
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    salario0 <- input$salario 
    if(salario0 < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario0 <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                  q = (1+incremento) , 
                  n = anios_aporte, 
                  i = interes,
                  type = "due")
    return(Ahorro)
    
  })
  # Carga de las tablas de mortalidad ----
  probsH<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",sheet=1)[,c(3)]))
  probsM<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",sheet=2)[,c(3)]))
  
  ls("package:lifecontingencies")
  TH <- probs2lifetable(probs=probsH, radix=100000, type="qx", name = "Mortalidad Hombres")
  TM <- probs2lifetable(probs=probsM, radix=100000, type="qx", name = "Mortalidad Mujeres")
  # Cálculo del valor actual de las prestaciones a otorgarse ----
  # load("C:/Users/MyHP/Desktop/TIC/Shiny-TIC/obtencion_pension_prom.Rdata") #Taynita
  load("obtencion_pension_prom.Rdata")
  
  # arreglo base de datos 
  # establecimiento minimo y maximo de pensiones
  pensiones2 <- pensiones2 %>%  mutate(prom_salario_a_usar = promedio_sueldo_real *(1.02154)^(2024 - as.numeric(format(as.Date(fecha_inicial_pension, '%Y %m %d'), "%Y"))), .before=promedio_sueldo_real)
  pensiones2 <- pensiones2 %>%  mutate(pension_final1 = pension_final *( 1.018261)^(5), .before=pension_final)
  
  pensiones2 <- pensiones2 %>% mutate(pensionesfinal2 = pension_final1)
  pensiones2 <- pensiones2 %>% dplyr::filter(tipo_seguro != 'SC')
  pensiones2 <- pensiones2 %>% dplyr::filter(estado_vigencia=="A")
  
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
  
  
  
  # Función de cálculo de la pensión promedio -----
  pension_promedio_fun <- function(e_j, impo, sexo1, sal_ini, anios_aporte){
    crec_salarios <- 0.02154
    
    res <- pensiones2 %>%  dplyr::filter(minimo(input$edad_inicio) <=edad_jubilacion & edad_jubilacion <= e_j + 3) %>%  
      dplyr::filter(impo + 24 >= numero_imposiciones &  numero_imposiciones >= ((minimo(input$edad_inicio) - input$edad_inicio)*12) ) %>% 
      dplyr::filter(sexo == sexo1) %>% 
      mutate(prom_salario_a_usar1 = ifelse(prom_salario_a_usar ==0, mean(pensiones2$prom_salario_a_usar[pensiones2$prom_salario_a_usar !=0]),prom_salario_a_usar)) %>% 
      dplyr::filter( Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]] - 200 <= prom_salario_a_usar1 & prom_salario_a_usar1 <= Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]]+200)
    #dplyr::filter( mean(sal_ini*(1+ 0.02154)^(seq(0, anios_aporte, by=1))) - 200 <= prom_salario_a_usar1 & prom_salario_a_usar1 <= mean(sal_ini*(1+ 0.02154)^(seq(0, anios_aporte, by=1)))+ 200)
    #-300, -400
    
    prom <- mean(res$pensionesfinal2)
    
    return(prom)
  }
  calcularVApensionesTotal <- reactive({
    SBU <- 460
    sexo <- input$sexo
    
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    anio_fin <- input$anio_inicio + (edad_jubilacion-edad_inicio)
    n_pensiones <- 100 - edad_jubilacion # 100 años de edad como límite
    anios_aporte <- edad_jubilacion-edad_inicio
    crec_pensiones <- 1.8261/100
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    crec_salarios <- 0.02154
    crec_SBU <- 0.02534
    salario <- input$salario
    #Cálculo de la pensión promedio
    
    pension_promedio <- Pension(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio))[[1]]
    
    # Calculo del VA de la pension
    i_12 <- (1+interes)^(1/12) - 1
    
    C <- pension_promedio* annuity(i = i_12, n=12, type = "due") 
    if(sexo == 'M'){
      va_pension <- C * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension <- C * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    E1 <- pension_promedio * (1 + i_12)^(-11)
    if(sexo == 'M'){
      va_pension1 <- E1 * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension1 <- E1 * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    E2 <- SBU * (1+ crec_SBU)^(anio_fin - 2024) * (1 + i_12)^(-11) 
    if(sexo == 'M'){
      va_pension2 <- E2 * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_SBU)/(1+crec_SBU), payment='due')
    }else{
      va_pension2 <- E2 * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_SBU)/(1+crec_SBU), payment='due')
      
    }
    
    
    pension <- pension_promedio  
    
    return(list(va_pension + va_pension1 + va_pension2 , pension))
    
  })
  
  # Outputs InfoBox ----
  # Colores para las cajas: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  output$ahorro <- renderValueBox({
    x <- calcularAhorroTotal()
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "Ahorro", icon = icon(name = "piggy-bank", class = "fa-solid fa-piggy-bank"),
      color = "purple", fill = TRUE
    )
  })
  output$pension_teorica_actual <- renderValueBox({
    x <- round(PensionActual(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio))[[1]], 2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "Pensión Teórica Actual", icon = icon(name = "calculator", class = "fa-solid fa-calculator"),
      color = "blue", fill = TRUE
    )
  })
  output$tasa_reemplazo <- renderValueBox({
    x <-  round( TasaReemplazo(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio)), 2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Tasa de reemplazo", icon = icon(name = "percent", class = "fa-solid fa-percent"),
      color = "aqua", fill = TRUE
    )
  })
  
  
  output$VApension <- renderValueBox({
    x <- round(calcularVApensionesTotal()[[1]],2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "VAA de la Pensión", icon = icon(name = "hand-holding-dollar", class = "fa-solid fa-hand-holding-dollar"),
      color = "purple", fill = TRUE
    )
  })
  output$pensionpromedio <- renderValueBox({
    x <- round(pension_promedio_fun(input$edad_jubilacion, (input$edad_jubilacion - input$edad_inicio)*12, input$sexo,  input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio)),2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = div(style = "white-space: normal; word-wrap: break-word; line-height: 1.2;", "Pensión Promedio Actual"),
      icon = icon(name = "users", class = "fa-solid fa-users"),
      color = "blue", fill = TRUE
    )
  })
  output$cobertura <- renderValueBox({
    x <- round(((calcularVApensionesTotal()[[1]]- calcularAhorroTotal())/calcularVApensionesTotal()[[1]])*100,1)
    if(x < 0){
      x <- 0
    }
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Aporte del Estado", icon = icon(name = "landmark", class = "fa-solid fa-landmark"),
      color = "aqua", fill = TRUE
    )
  })
  
  
  # Evolucion de la reserva del individuo ----
  
  Evolucion_Reservas_con_aporte40 <- reactive({
    # Parámetros  
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    
    # Crecimiento de la reserva ( cotizaciones )
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones )
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    # con el 40% del estado
    ev_res2 <- data.frame(
      anio = c((anio_fin+1):anio_muerte),
      edad = c(edad_jubilacion:100),
      res_acum = numeric(num_anios_pension),
      gasto_anio = numeric(num_anios_pension)
    )
    
    x=edad_jubilacion
    m = 12
    i = crec_pensiones
    #i = crec_pensiones
    sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
    ahorro_total_inicial
    
    for (j in c(1:num_anios_pension)) {
      
      
      if (sexo=="M") {
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 
          
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
        }
      }else{
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 
          
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
        }
        
      }
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
    
  })
  Evolucion_Reservas_SIN_aporte <- reactive({
    # Parámetros  
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    
    # Crecimiento de la reserva ( cotizaciones ) 
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) 
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    # SIN el 40% del estado
    ev_res2 <- data.frame(
      anio = c((anio_fin+1):anio_muerte),
      edad = c(edad_jubilacion:100),
      res_acum = numeric(num_anios_pension),
      gasto_anio = numeric(num_anios_pension)
    )
    
    x=edad_jubilacion
    m = 12
    i = crec_pensiones
    #i = crec_pensiones
    sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
    ahorro_total_inicial
    
    for (j in c(1:num_anios_pension)) {
      if (sexo=="M") {
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"] # Falta capitalizar el ahorro alcanzado
          #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
        }
      }else{
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
          
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
        }
        
      }
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
    
  })
  
  output$evolucion_reservas_con_aporte <- renderHighchart({
    
    edad <- Evolucion_Reservas_con_aporte40()[[1]]
    reservas <- Evolucion_Reservas_con_aporte40()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo") %>%
      hc_subtitle(text = "Con el aporte del 40% del Estado en cada pensión") %>% 
      hc_title(text = "Evolución de Reservas por Edad") %>%
      hc_subtitle(text = paste("con el aporte del 40% del Estado en cada pensión")) %>%
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })
  
  fun_anio_deficit<- function (ahorro, va_pensiones, pension_inicial, porcentaje_estado){
    
    ahorro <- ahorro 
    crec_pensiones <- 1.8261/100
    interes <- input$interes/100
    i_12 <- (1+(input$interes/100))^(1/12) - 1
    
    pension <- pension_inicial * (1-porcentaje_estado/100) * annuity(i = i_12, n=12, type = "due") 
    i <- 1
    
    while((ahorro - pension) > 0){
      ahorro <- (ahorro - pension)* (1 + interes)
      pension <- pension * (1+ crec_pensiones)
      i <- i + 1
      if (!is.finite(ahorro)) {
        return(110) 
      }
      
    }
    
    return(i + input$edad_jubilacion)
  }
  
  output$deficit_porcentaje <- renderHighchart({
    y <- c()
    for (k in 0:40){
      y <- c(y , fun_anio_deficit(calcularAhorroTotal(), calcularVApensionesTotal()[[1]] ,calcularVApensionesTotal()[[2]] , k))
    }
    
    data <- data.frame(
      anio = y,
      porcentaje = seq(0, 40, by = 1)
    )
    
    # Crear gráfico de Highcharts
    data %>% 
      hchart(., 
             type = "line", 
             name= 'Porcentaje de Aportación del Estado',
             hcaes(x = anio, 
                   y = porcentaje)) %>% 
      hc_title(text = "Impacto del Porcentaje de Aportación del Estado en la Sostenibilidad de las Pensiones") %>% 
      hc_subtitle(text = "Edad del jubilado al agotarse los recursos para el pago de su pensión según el porcentaje de aporte del Estado") %>% 
      hc_xAxis(title = list(text = "Edad del jubilado donde se alcanza el déficit")) %>%  
      hc_yAxis(title = list(text = "Porcentaje de Aportación del Estado")) %>% 
      hc_add_theme(hc_theme_elementary()) %>%
      hc_tooltip(pointFormat = 'Edad del jubilado: {point.x}<br>Porcentaje de Aportación del Estado: {point.y}') 
  })
  
  # Graficos interactivos para primeros resultados ----
  
  # Tiempo en el Sistema de Seguridad Social 
  output$tiempo_sss <- renderHighchart({
    
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- ifelse(input$edad_jubilacion>=60,input$edad_jubilacion,60)
    anio_ini <- input$anio_inicio
    anio_fin <- anio_ini + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    colores <- c('#605CA8', '#0073B7')
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Tiempo del Individuo en el Sistema de Seguridad Social") %>%
      hc_subtitle(text = "Número de años del Individuo como cotizante y como pensionista en el IESS") %>% 
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = TRUE, format = '{point.percentage:.1f} %'),
        showInLegend = TRUE
      )) %>%
      hc_series(list(
        name = "Años",
        colorByPoint = TRUE,
        data = list(
          list(name = "Cotizante", y = num_anios, color = colores[1]),
          list(name = "Pensionista", y = num_anios_pension, color = colores[2])
        )
      )) 
  })
  # Porcentaje de cobertura del estado en prestaciones pension
  output$porc_cobertura <- renderHighchart({
    
    porc_estado <- round(((calcularVApensionesTotal()[[1]]- calcularAhorroTotal())/calcularVApensionesTotal()[[1]])*100,1)
    vaa_prestacion <- round(calcularVApensionesTotal()[[1]],1)
    vaa_prestacion_estado <- vaa_prestacion*(porc_estado/100)
    vaa_prestacion_iess <- vaa_prestacion*(1-(porc_estado/100))
    
    colores <- c('#605CA8', '#0073B7')
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Cobertura del Estado Ecuatoriano sobre las prestaciones del Jubilado") %>%
      hc_subtitle(text = "Porcentaje aportado por el Estado Ecuatoriano para el pago de pensiones del Jubilado") %>% 
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = TRUE, format = '{point.percentage:.1f} %'),
        showInLegend = TRUE
      )) %>%
      hc_series(list(
        name = "Dólares",
        colorByPoint = TRUE,
        data = list(
          list(name = "Prestaciones Cubiertas por el IESS", y = vaa_prestacion_iess, color = colores[1]),
          list(name = "Prestaciones Cubiertas por el Estado", y = vaa_prestacion_estado, color = colores[2])
        )
      )) 
  })
  
  # TABLA -----------
  Ahorro <- function(edad_inicio, inflacion, interes, edad_jubilacion, salario, anio_inicio){
    inflacion <-  inflacion/ 100 
    interes <-  interes / 100
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    salario0 <- salario
    
    if(salario0 < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario0 <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+incremento) , 
                  n = anios_aporte, 
                  i = interes,
                  type = "due")
    return(Ahorro)
  }
  VApensiones_fun <- function(sexo,edad_inicio,edad_jubilacion, inflacion, interes,salario, anio_inicio){
    n_pensiones <- 100 - edad_jubilacion # 100 años de edad como límite
    anios_aporte <- edad_jubilacion-edad_inicio
    crec_pensiones <- 1.8261/100
    inflacion <- inflacion / 100 
    interes <- interes /100
    crec_salarios <- 0.02154
    
    pension_promedio <- Pension(edad_inicio, salario , (edad_jubilacion - edad_inicio))[[1]]
    
    # Calculo del VA de la pension
    i_12 <- (1+interes)^(1/12) - 1
    
    C <- pension_promedio * annuity(i = i_12, n=12, type = "due")
    if(sexo == 'M'){
      va_pension <- C * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension <- C * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    pension <- pension_promedio  
    
    return(list(va_pension, pension))
  }
  Evolucion_Reservas_fun <- function(sexo, edad_inicio, edad_jubilacion,salario_ini, anio_inicio, interes,aporte = TRUE){
    # Parámetros  
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    i_actuarial <- interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    # Crecimiento de la reserva ( cotizaciones ) -----------------------------------
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) -------------------------------------
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    if(aporte == TRUE){
      # con el 40% del estado
      ev_res2 <- data.frame(
        anio = c((anio_fin+1):anio_muerte),
        edad = c(edad_jubilacion:100),
        res_acum = numeric(num_anios_pension),
        gasto_anio = numeric(num_anios_pension)
      )
      
      x=edad_jubilacion
      m=12
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
    }else{
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
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
  }
  
  output$tabla_pensiones <- renderReactable({
    # Creación de las columnas
    edad_jubilacion <- seq(input$edad_jubilacion, input$edad_jubilacion + 10)
    anos_aportacion <- edad_jubilacion - input$edad_inicio
    fondo_acumulado <- sapply(edad_jubilacion, function(edad) {
      Ahorro(input$edad_inicio, input$inflacion, input$interes, edad, input$salario, input$anio_inicio)
    })
    fondo_acumulado <- format(fondo_acumulado, big.mark = ",", decimal.mark = ".", nsmall = 2)
    fondo_acumulado <- paste("USD", fondo_acumulado)
    
    ultimo_anio_reserva_con_aporte <- sapply(edad_jubilacion, function(edad_jub) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun(input$sexo, input$edad_inicio,
                                                           edad_jub, input$salario,
                                                           input$anio_inicio, input$interes,
                                                           aporte = TRUE)[[2]] > 0)) - 2
    })
    ultimo_anio_reserva_sin_aporte <- sapply(edad_jubilacion, function(edad_jub) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun(input$sexo, input$edad_inicio,
                                                           edad_jub, input$salario,
                                                           input$anio_inicio, input$interes,
                                                           aporte = FALSE)[[2]] > 0)) - 2
    })
    pension_anual <- sapply(edad_jubilacion, function(edad) {
      pension_base <- Pension(input$edad_inicio, input$salario, (edad - input$edad_inicio))[[1]] * 12
    })
    pension_mensual <- pension_anual / 12
    pension_anual <- format(pension_anual, big.mark = ",", decimal.mark = ".", nsmall = 2)
    pension_anual <- paste("USD", pension_anual)
    pension_mensual <- format(pension_mensual, big.mark = ",", decimal.mark = ".", nsmall = 2)
    pension_mensual <- paste("USD", pension_mensual)
    
    tasa_reemplazo <- sapply(edad_jubilacion, function(edad) {
      round(TasaReemplazo(input$edad_inicio, input$salario, (edad - input$edad_inicio)), 2)
    })
    
    # Creación de la tabla final
    tabla <- data.frame(
      `Edad de jubilación` = edad_jubilacion,
      `Años de aportación` = anos_aportacion,
      `Fondo acumulado` = fondo_acumulado,
      `Pensión de jubilación anual` = pension_anual,
      `Pensión de jubilación mensual` = pension_mensual,
      `Último año con reserva positiva con aporte del estado` = ultimo_anio_reserva_con_aporte,
      `Último año con reserva positiva sin aporte del estado` = ultimo_anio_reserva_sin_aporte,
      `Tasa de Reemplazo` = tasa_reemplazo
    )
    
    colnames(tabla) <- c("Edad de Jubilación",
                         "Años de aportación",
                         "Fondo acumulado",
                         "Pensión de jubilación anual",
                         "Pensión de jubilación mensual",
                         "Último año con reserva positiva con aporte del estado",
                         "Último año con reserva positiva sin aporte del estado",
                         "Tasa de Reemplazo")
    
    # Convertir la tasa de reemplazo en una barra de progreso
    tabla$`Tasa de Reemplazo` <- sapply(tabla$`Tasa de Reemplazo`, function(x) {
      percent <- as.numeric(x) / 100
      formatted_percent <- sprintf("%.2f", x)
      bar <- paste0('<div style="background-color: #f2f2f2; border-radius: 5px; width: 100px; display: inline-block;">',
                    '<div style="background-color: #4CAF50; width: ', percent * 100, '%; height: 15px; border-radius: 5px;"></div>',
                    '</div> ', formatted_percent, '%')
      bar
    })
    
    # Creación de la tabla con reactable
    reactable(
      tabla,
      columns = list(
        `Edad de Jubilación` = colDef(align = "center"),
        `Años de aportación` = colDef(align = "center"),
        `Fondo acumulado` = colDef(align = "center", style = list(color = "green", fontWeight = "bold")),
        `Pensión de jubilación anual` = colDef(align = "center"),
        `Pensión de jubilación mensual` = colDef(align = "center", style = list(color = "#110C87", fontWeight = "bold")),
        `Último año con reserva positiva con aporte del estado` = colDef(align = "center"),
        `Último año con reserva positiva sin aporte del estado` = colDef(align = "center"),
        `Tasa de Reemplazo` = colDef(html = TRUE, align = "center", minWidth = 110, headerStyle = list(textAlign = "center"))
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 6,
      theme = reactableTheme(
        headerStyle = list(backgroundColor = "#0073B7", color = "white", fontWeight = "bold"),
        borderColor = "#d9d9d9"
      )
    )
  })
  
  # REFORMA ANDREA ---- 
  Pension_reforma <- function(edad, salario, anios_aporte, anios_prom){
    
    if(salario < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    if (anios_prom <= anios_aporte){
      mejores_n_salarios <- sapply(1:anios_prom, function(i) {
        salario * (1 + incremento)^(anios_aporte - i)
      })
      prom <- sum(mejores_n_salarios) / anios_prom
    }
    else{
      mejores_n_salarios <- sapply(1:anios_aporte, function(i) {
        salario * (1 + incremento)^(anios_aporte - 1)
      })
      prom <- sum(mejores_n_salarios) / anios_aporte
    }
    
    # pension = promedio * coef
    coef <- Coeficiente$Coef[Coeficiente$An.Imposiciones == anios_aporte]
    
    pension <- prom * coef 
    
    pension <- ajustar_pension_min(anios_aporte*12, pension, rango_valores_min())
    pension <- ajustar_pension_max(anios_aporte*12, pension, rango_valores_max())
    
    return(list(pension, prom))
  }
  PensionActual_reforma <- function(edad, salario, anios_aporte, anios_prom){
    
    if(salario < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }  
    
    if (anios_prom <= anios_aporte){
      mejores_n_salarios <- sapply(1:anios_prom, function(i) {
        salario * (1 + incremento)^(anios_aporte - i)
      })
      prom <- sum(mejores_n_salarios) / anios_prom
    }
    else{
      mejores_n_salarios <- sapply(1:anios_aporte, function(i) {
        salario * (1 + incremento)^(anios_aporte - 1)
      })
      prom <- sum(mejores_n_salarios) / anios_aporte
    }
    
    coef <- Coeficiente$Coef[Coeficiente$An.Imposiciones == anios_aporte]
    
    pension <- prom * coef 
    
    pension <- ajustar_pension_min(anios_aporte*12, pension, rango_valores_min_A())
    pension <- ajustar_pension_max(anios_aporte*12, pension, rango_valores_max_A())
    
    return(list(pension, prom))
  }
  TasaReemplazo_reforma <- function(edad, salario, anios_aporte, anios_prom){
    
    incremento <- 0.025339
    
    pension <- Pension_reforma(edad, salario, anios_aporte, anios_prom)[[1]]
    ultimo_sueldo <- salario * (1+incremento)^(anios_aporte - 1)
    
    tasa <- (pension / ultimo_sueldo) * 100
    return(tasa)
  }
  VApensionesTotal_reforma <- function(sexo, edad_inicio, edad_jubilacion, anio_inicio, inflacion, interes, salario, anios_prom){
    SBU <- 460
    
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio)
    n_pensiones <- 100 - edad_jubilacion # 100 años de edad como límite
    anios_aporte <- edad_jubilacion-edad_inicio
    crec_pensiones <- 1.8261/100
    inflacion <- inflacion / 100 
    interes <-  interes/100
    crec_salarios <- 0.02154
    crec_SBU <- 0.02534
    
    #Cálculo de la pensión promedio
    pension_promedio <- Pension_reforma(edad_inicio, salario, (edad_jubilacion - edad_inicio), anios_prom)[[1]]
    
    # Calculo del VA de la pension
    i_12 <- (1+interes)^(1/12) - 1
    
    C <- pension_promedio* annuity(i = i_12, n=12, type = "due") 
    if(sexo == 'M'){
      va_pension <- C * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension <- C * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    E1 <- pension_promedio * (1 + i_12)^(-11)
    if(sexo == 'M'){
      va_pension1 <- E1 * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
    }else{
      va_pension1 <- E1 * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_pensiones)/(1+crec_pensiones), payment='due')
      
    }
    
    E2 <- SBU * (1+ crec_SBU)^(anio_fin - 2024) * (1 + i_12)^(-11) 
    if(sexo == 'M'){
      va_pension2 <- E2 * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_SBU)/(1+crec_SBU), payment='due')
    }else{
      va_pension2 <- E2 * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-crec_SBU)/(1+crec_SBU), payment='due')
      
    }
    
    
    pension <- pension_promedio  
    
    return(list(va_pension + va_pension1 + va_pension2 , pension))
  }
  Evolucion_Reservas_fun_reforma <- function(sexo, edad_inicio, edad_jubilacion,salario_ini, anio_inicio, interes, num_prom,aporte = TRUE){
    # Parámetros  
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    i_actuarial <- interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    # Crecimiento de la reserva ( cotizaciones ) -----------------------------------
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) -------------------------------------
    
    pen <- Pension_reforma(edad_inicio, salario_ini, num_anios, num_prom)[[1]]
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    if(aporte == TRUE){
      # con el 40% del estado
      ev_res2 <- data.frame(
        anio = c((anio_fin+1):anio_muerte),
        edad = c(edad_jubilacion:100),
        res_acum = numeric(num_anios_pension),
        gasto_anio = numeric(num_anios_pension)
      )
      
      x=edad_jubilacion
      m = 12
      i = crec_pensiones
      #i = crec_pensiones
      sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
      ahorro_total_inicial
      
      for (j in c(1:num_anios_pension)) {
        
        
        if (sexo=="M") {
          if (j==1) {
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 # Falta capitalizar el ahorro alcanzado
            #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
          }else{
            
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
          }
        }else{
          if (j==1) {
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 # Falta capitalizar el ahorro alcanzado
            #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
          }else{
            
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
          }
          
        }
      }
    }else{
      # SIN el 40% del estado
      ev_res2 <- data.frame(
        anio = c((anio_fin+1):anio_muerte),
        edad = c(edad_jubilacion:100),
        res_acum = numeric(num_anios_pension),
        gasto_anio = numeric(num_anios_pension)
      )
      
      x=edad_jubilacion
      m = 12
      i = crec_pensiones
      #i = crec_pensiones
      sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
      ahorro_total_inicial
      
      for (j in c(1:num_anios_pension)) {
        
        
        if (sexo=="M") {
          if (j==1) {
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"] # Falta capitalizar el ahorro alcanzado
            #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
          }else{
            
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
          }
        }else{
          if (j==1) {
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"] # Falta capitalizar el ahorro alcanzado
            #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
          }else{
            
            doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
            dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
            dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
            
            ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
            ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
          }
          
        }
      }
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
  }
  
  output$tasa_reemplazo_reforma <- renderValueBox({
    x <-  round( TasaReemplazo_reforma(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio), input$anios_calculo_pension), 2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Tasa de reemplazo", icon = icon(name = "percent", class = "fa-solid fa-percent"),
      color = "purple", fill = TRUE
    )
  })
  output$cobertura_reforma <- renderValueBox({
    VAApensiones <- VApensionesTotal_reforma(input$sexo, input$edad_inicio, input$edad_jubilacion, input$anio_inicio,
                                             input$inflacion, input$interes, input$salario, input$anios_calculo_pension)[[1]]
    x <- round(((VAApensiones - calcularAhorroTotal())/VAApensiones)*100,1)
    if(x < 0){
      x <- 0
    }
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Aporte del Estado", icon = icon(name = "landmark", class = "fa-solid fa-landmark"),
      color = "blue", fill = TRUE
    )
  })
  output$pension_teorica_actual_reforma <- renderValueBox({
    x <- round(PensionActual_reforma(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio), input$anios_calculo_pension)[[1]], 2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = div(style = "white-space: normal; word-wrap: break-word; line-height: 1.2;", "Pensión Teoríca Actual"),
      icon = icon(name = "calculator", class = "fa-solid fa-calculator"),
      color = "aqua", fill = TRUE
    )
  })
  output$pension_teorica_jub_reforma <- renderValueBox({
    x <- round(Pension_reforma(input$edad_inicio, input$salario , (input$edad_jubilacion - input$edad_inicio), input$anios_calculo_pension)[[1]], 2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = div(style = "white-space: normal; word-wrap: break-word; line-height: 1.2;", "Pensión Teoríca a la Jubilación"),
      icon = icon(name = "money-bill-trend-up", class = "fa-solid fa-money-bill-trend-up"),
      color = "aqua", fill = TRUE
    )
  })
  output$VApension_reforma <- renderValueBox({
    x <- round(VApensionesTotal_reforma(input$sexo, input$edad_inicio, input$edad_jubilacion, input$anio_inicio,
                                        input$inflacion, input$interes, input$salario, input$anios_calculo_pension)[[1]],2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "VAA de la Pensión", icon = icon(name = "hand-holding-dollar", class = "fa-solid fa-hand-holding-dollar"),
      color = "teal", fill = TRUE
    )
  })
  
  
  output$evolucion_reservas_con_aporte_con_reforma <- renderHighchart({
    edad <- Evolucion_Reservas_fun_reforma(input$sexo, input$edad_inicio,
                                           input$edad_jubilacion, input$salario,
                                           input$anio_inicio, input$interes,
                                           input$anios_calculo_pension, aporte = TRUE)[[1]]
    reservas <- Evolucion_Reservas_fun_reforma(input$sexo, input$edad_inicio,
                                               input$edad_jubilacion, input$salario,
                                               input$anio_inicio, input$interes,
                                               input$anios_calculo_pension, aporte = TRUE)[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución de Reservas por Edad") %>%
      hc_subtitle(text = paste("con el aporte del 40% del Estado en cada pensión y considerando los", input$anios_calculo_pension,"mejores años de aporte")) %>%
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })
  output$pension_vs_años_con_reforma <- renderHighchart({
    anio_aplicacion <- seq(2024, 2049)
    anios_considerados <- seq(5,30)
    pension_mensual <- sapply(anios_considerados, function(n) {
      round(PensionActual_reforma(input$edad_inicio, 
                                  input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), 
                                  (input$edad_jubilacion - input$edad_inicio), 
                                  n)[[1]], 2)
    })
    
    tabla <- data.frame(
      `Año de Aplicación` = anio_aplicacion,
      `Años considerados para el cálculo` = anios_considerados,
      `Pensión de jubilación mensual` = pension_mensual
    )
    
    # Crear el gráfico de Highcharts
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Pensión de Jubilación Mensual por Año de Aplicación") %>%
      hc_xAxis(categories = anio_aplicacion, title = list(text = "Año de Aplicación")) %>%
      hc_yAxis(title = list(text = "Pensión de Jubilación Mensual")) %>%
      hc_add_series(name = "Pensión Mensual", data = pension_mensual, color = "#8D58E5") %>%
      hc_tooltip(pointFormat = "Pensión: {point.y:.2f}") %>% 
      hc_legend(enabled = FALSE) 
    
  })
  
  output$tabla_pensiones_reformaABC <- renderReactable({
    # Creación de las columnas
    anio <- seq(2024, 2049)
    anios_prom <- seq(5,30)
    pension_mensual <- sapply(anios_prom, function(n) {
      round(PensionActual_reforma(input$edad_inicio, 
                                  input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), 
                                  (input$edad_jubilacion - input$edad_inicio), 
                                  n)[[1]], 2)
    })
    pension_mensual <- format(pension_mensual, big.mark = ",", decimal.mark = ".", nsmall = 2)
    pension_mensual <- paste("USD", pension_mensual)
    
    ultimo_anio_reserva_con_aporte <- sapply(anios_prom, function(num) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun_reforma(input$sexo, input$edad_inicio,
                                                                   input$edad_jubilacion, input$salario,
                                                                   input$anio_inicio, input$interes,
                                                                   num, aporte = TRUE)[[2]] > 0))-2
      
    })
    ultimo_anio_reserva_sin_aporte <- sapply(anios_prom, function(num) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun_reforma(input$sexo, input$edad_inicio,
                                                                   input$edad_jubilacion, input$salario,
                                                                   input$anio_inicio, input$interes, num,
                                                                   aporte = FALSE)[[2]] > 0))-2
      
    })
    
    tasa_reemplazo <- sapply(anios_prom, function(num) {
      round(TasaReemplazo_reforma(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio), num), 2)
    })
    
    # Creación de la tabla final
    tabla <- data.frame(
      `Año de Aplicación` = anio,
      `Años considerados para el cálculo` = anios_prom,
      `Pensión de jubilación mensual` = pension_mensual,
      `Último año con reserva positiva con aporte del estado` = ultimo_anio_reserva_con_aporte,
      `Último año con reserva positiva sin aporte del estado` = ultimo_anio_reserva_sin_aporte,
      `Tasa de Reemplazo` = tasa_reemplazo
    )
    
    colnames(tabla) <- c("Año de Aplicación",
                         'Años considerados para el cálculo',
                         'Pensión de jubilación mensual',
                         'Último año con reserva positiva con aporte del estado',
                         'Último año con reserva positiva sin aporte del estado',
                         'Tasa de Reemplazo'
    )
    
    # Convertir la tasa de reemplazo en una barra de progreso
    tabla$`Tasa de Reemplazo` <- sapply(tabla$`Tasa de Reemplazo`, function(x) {
      percent <- as.numeric(x) / 100
      formatted_percent <- sprintf("%.2f", x)
      bar <- paste0('<div style="background-color: #f2f2f2; border-radius: 5px; width: 100px; display: inline-block;">',
                    '<div style="background-color: #4CAF50; width: ', percent * 100, '%; height: 15px; border-radius: 5px;"></div>',
                    '</div> ', formatted_percent, '%')
      bar
    })
    
    # Creación de la tabla con reactable
    reactable(
      tabla,
      columns = list(
        `Año de Aplicación` = colDef(align = "center"),
        `Años considerados para el cálculo` = colDef(align = "center"),
        `Pensión de jubilación mensual` = colDef(align = "center", style = list(color = "green", fontWeight = "bold")),
        `Último año con reserva positiva con aporte del estado` = colDef(align = "center"),
        `Último año con reserva positiva sin aporte del estado` = colDef(align = "center"),
        `Tasa de Reemplazo` = colDef(html = TRUE, align = "center", minWidth = 110, headerStyle = list(textAlign = "center"))
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 6,
      theme = reactableTheme(
        headerStyle = list(backgroundColor = "#0073B7", color = "white", fontWeight = "bold"),
        borderColor = "#d9d9d9"
      )
    )
  })
  
  
  # REFORMA AMMY ---------------------------------------------------------------
  
  calcularAhorroTotal_01<- reactive({

    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))

    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
      # Aportación extra del décimo tercer y décimo cuarto sueldo
      ev_res[j,3] <- ev_res[j,3] + VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                                       q = (1+incremento) , 
                                       n = j , #años de aporte hasta el momento
                                       i = i_actuarial,
                                       type = "due")
    }
    
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    return(ahorro_total_inicial)
  })
  
  Num_total_cotizaciones_dec_3_4 <- reactive({
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion

    num_anios <- edad_jubilacion - edad_inicio
    num_cotizaciones_antes <- 12*num_anios
    num_cotizaciones_despues <- 14*num_anios
    
    return(list(num_anios,num_cotizaciones_antes,num_cotizaciones_despues))

  })
  
  ### General ----
  
  output$comparación_ahorro_total <- renderHighchart({
    
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    
    # Crecimiento de la reserva ( cotizaciones )
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios),
      res_acum_2 = numeric(num_anios)
    )
    
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
      ev_res[j,4] <- ev_res[j,3] + VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                                       q = (1+incremento) , 
                                       n = j , #años de aporte hasta el momento
                                       i = i_actuarial,
                                       type = "due")
    }
    
    datos1 <- ev_res %>% select("anio","res_acum"  ) %>% mutate(ahorro = res_acum, variable = "Ahorro actual")  %>% select("anio","ahorro", variable)
    datos2 <- ev_res %>% select("anio","res_acum_2") %>% mutate(variable = "Ahorro Posterior a la Reforma", ahorro = res_acum_2) %>% select("anio","ahorro", variable)
    datos <- rbind(datos1,datos2)
    
    hchart(datos,
           "line",
           hcaes(x = anio, y = ahorro, group = variable)
    ) %>% 
      hc_title(text = "Comparación de la evolución del ahorro antes y después de la reforma",style = list(fontSize = '12px'))%>%
      hc_xAxis(title = list(text = "Año")) %>%
      hc_yAxis(title = list(text = "Ahorro acumulado"))%>%
      hc_colors(c("#f84424", "#009EE0")) %>% 
      hc_plotOptions(
        line = list(
          marker = list(
            radius = 1# Tamaño de las bolitas
          ),
          lineWidth = 0.5 # Grosor de la línea
        )
      )%>%
      hc_tooltip(pointFormat = '{point.y:.2f}')
  })
  
  output$num_coti_chart <- renderHighchart({
    
    df <- data.frame(
      stringsAsFactors = FALSE,
      name = c(
        "Número de Aportaciones Extra",
        "Número de Aportaciones Previo a la Reforma "
      ),
      count = c(Num_total_cotizaciones_dec_3_4()[[3]]-Num_total_cotizaciones_dec_3_4()[[2]],
                Num_total_cotizaciones_dec_3_4()[[2]]),
      col = c("#f84424", "#009EE0"#,
              # "#BE3075", "#EB001F", "#64A12D", "#FFED00",
              # "#000000", "#008AC5", "#009EE0"
      ),
      abbrv = c("Aportaciones Extra", "Aportaciones Iniciales")
    )
    
    hchart(
      df,
      "item",
      hcaes(name = name,
            y = count,
            label = abbrv,
            color = col
      ),
      name = "Aportaciones",
      showInLegend = TRUE,
      size = "100%",
      center = list("50%", "75%")
    ) %>%
      hc_title(text = "Número de cotizaciones Totales del Individuo",style = list(fontSize = '12px')) %>%
      hc_legend(labelFormat = '{name} <span style="opacity: 0.4">{y}</span>')%>%
      hc_plotOptions(
        series = list(
          marker = list(
            radius = 4 # Ajusta el tamaño de las bolitas
          )
        )
      )
  })
  
  ### Escenario 1 ----
  
  output$pension_teorica_actual_01 <- renderValueBox({
    x <- round(PensionActual(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio))[[1]], 2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "Pensión Teórica Actual", icon = icon(name = "calculator", class = "fa-solid fa-calculator"),
      color = "blue", fill = TRUE
    )
  })
  
  output$tasa_reemplazo_01 <- renderValueBox({
    x <-  round( TasaReemplazo(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio)), 2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Tasa de reemplazo", icon = icon(name = "percent", class = "fa-solid fa-percent"),
      color = "aqua", fill = TRUE
    )
  })
  
  output$VApension_01 <- renderValueBox({
    x <- round(calcularVApensionesTotal()[[1]],2)
    x <- format(x, big.mark = ",", decimal.mark = ".", nsmall=2)
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste0("$", x)), 
      title = "VAA de la Pensión", icon = icon(name = "hand-holding-dollar", class = "fa-solid fa-hand-holding-dollar"),
      color = "purple", fill = TRUE
    )
  })
  
  output$cobertura_01 <- renderValueBox({
    x <- round(((calcularVApensionesTotal()[[1]]- calcularAhorroTotal_01())/calcularVApensionesTotal()[[1]])*100,1)
    if(x < 0){
      x <- 0
    }
    shinydashboard::infoBox( 
      value = div(style = "color: white", paste(x,"%")), 
      title = "Aporte del Estado", icon = icon(name = "landmark", class = "fa-solid fa-landmark"),
      color = "aqua", fill = TRUE
    )
  })
  
  # Graficos de evolución
  
          # Antes
  output$evolucion_reservas_con_aporte_sin_decimos <- renderHighchart({
    
    edad <- Evolucion_Reservas_con_aporte40()[[1]]
    reservas <- Evolucion_Reservas_con_aporte40()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo") %>%
      hc_subtitle(text = "evolucion_reservas_con_aporte_sin_decimos") %>% 
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })        
  output$evolucion_reservas_sin_aporte_sin_decimos <- renderHighchart({
    
    edad <- Evolucion_Reservas_SIN_aporte()[[1]]
    reservas <- Evolucion_Reservas_SIN_aporte()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo") %>%
      hc_subtitle(text = "evolucion_reservas_sin_aporte_sin_decimos") %>% 
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })
          # Antes: En conjunto
  output$evolucion_reservas_con_y_sin_aporte_sin_decimos <- renderHighchart({
    
    edad <- Evolucion_Reservas_SIN_aporte()[[1]]
    reservas_sin <- Evolucion_Reservas_SIN_aporte()[[2]]
    reservas_con <- Evolucion_Reservas_con_aporte40()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos_sin <- max(which(reservas_sin > 0))
    first_neg_sin <- min(which(reservas_sin < 0))
    
    last_pos_con <- max(which(reservas_con > 0))
    first_neg_con <- min(which(reservas_con < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo",
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text = "Evolucion reservas con y sin aporte del Estado, y <b> sin considerar la reforma </b>",
                  style = list(fontSize = "11px")) %>% 
      hc_xAxis(categories = edad, title = list(text = "Edad")) %>%
      hc_yAxis(title = list(text = "Reservas")) %>%
      hc_tooltip(shared = TRUE, valueDecimals = 0) %>%
      hc_plotOptions(area = list(
        marker = list(enabled = FALSE),
        enableMouseTracking = TRUE
      )) %>%
      hc_add_series(
        name = "Aportación",
        data = reservas_sin[1:max_res],  # datos positivos hasta 150000
        color = "blue",
        fillOpacity = 0.3
      ) %>%
      hc_add_series(
        name = "Jubilación sin considerar aporte estatal",
        data = c(rep(NA, max_res-1), reservas_sin[max_res:last_pos_sin]),  # datos decreciendo de 150000 a 0
        color = "green",
        fillOpacity = 0.3
      ) %>%
      hc_add_series(
        name = "Jubilación (déficit) sin considerar aporte estatal",
        data = c(rep(NA, last_pos_sin-1), reservas_sin[(last_pos_sin):length(reservas_sin)]),  # datos decreciendo de 0 a -150000
        color = "red",
        fillOpacity=0.3
      ) %>%
      hc_add_series(
        name = "Jubilación considerando aporte estatal",
        data = c(rep(NA, max_res-1), reservas_con[max_res:last_pos_con]),  # datos decreciendo de 150000 a 0
        color = "#747474",
        type = "line",
        dashStyle = "dot"
      ) %>%
      hc_add_series(
        name = "Jubilación (déficit) considerando aporte estatal",
        data = c(rep(NA, last_pos_con-1), reservas_con[(last_pos_con):length(reservas_con)]),  # datos decreciendo de 0 a -150000
        color = "#747474",
        type = "line",
        dashStyle = "dot"
      ) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
          # Despues
  Evolucion_Reservas_con_aporte40_con_decimos <- reactive({
    # Parámetros  
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    
    # Crecimiento de la reserva ( cotizaciones ) 
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
      # Aportación extra del décimo tercer y décimo cuarto sueldo
      ev_res[j,3] <- ev_res[j,3] + VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                                       q = (1+incremento) , 
                                       n = j , #años de aporte hasta el momento
                                       i = i_actuarial,
                                       type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) 
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    # con el 40% del estado
    ev_res2 <- data.frame(
      anio = c((anio_fin+1):anio_muerte),
      edad = c(edad_jubilacion:100),
      res_acum = numeric(num_anios_pension),
      gasto_anio = numeric(num_anios_pension)
    )
    
    x=edad_jubilacion
    m = 12
    i = crec_pensiones
    #i = crec_pensiones
    sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
    ahorro_total_inicial
    
    for (j in c(1:num_anios_pension)) {
      
      
      if (sexo=="M") {
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 # Falta capitalizar el ahorro alcanzado
          #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
        }
      }else{
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6 # Falta capitalizar el ahorro alcanzado
          #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]*0.6
        }
        
      }
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
    
  })
  output$evolucion_reservas_con_aporte_con_decimos <- renderHighchart({
    
    edad <-  Evolucion_Reservas_con_aporte40_con_decimos()[[1]]
    reservas <-  Evolucion_Reservas_con_aporte40_con_decimos()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo") %>%
      hc_subtitle(text = "evolucion_reservas_con_aporte_con_decimos") %>% 
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })
  Evolucion_Reservas_sin_aporte40_con_decimos <- reactive({
    # Parámetros  
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- input$edad_jubilacion
    salario_ini <- input$salario
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    crec_SBU <- 2.5339/100; crec_SBU_12 <- (1+crec_SBU)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_inicio <- input$anio_inicio
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    
    i_actuarial <- input$interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    
    # Crecimiento de la reserva ( cotizaciones ) 
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
      # Aportación extra del décimo tercer y décimo cuarto sueldo
      ev_res[j,3] <- ev_res[j,3] + VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                                       q = (1+incremento) , 
                                       n = j , #años de aporte hasta el momento
                                       i = i_actuarial,
                                       type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) 
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    # con el 40% del estado
    ev_res2 <- data.frame(
      anio = c((anio_fin+1):anio_muerte),
      edad = c(edad_jubilacion:100),
      res_acum = numeric(num_anios_pension),
      gasto_anio = numeric(num_anios_pension)
    )
    
    x=edad_jubilacion
    m = 12
    i = crec_pensiones
    #i = crec_pensiones
    sbu_anio_jub <- 460*(1+crec_SBU)^(anio_fin-2024+1)
    ahorro_total_inicial
    
    for (j in c(1:num_anios_pension)) {
      
      
      if (sexo=="M") {
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"] # Falta capitalizar el ahorro alcanzado
          #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TH, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TH,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TH,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
        }
      }else{
        if (j==1) {
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ahorro_total_inicial*(1+i_actuarial) - ev_res2[j,"gasto_anio"] # Falta capitalizar el ahorro alcanzado
          #PROFE: Aqui no entiendo por que le capitaliza, si ese valor de ahorro_total_inicial ya esta al final del ultimo, y estos gastos estan evaluados al principio de la edad de jubilacion
        }else{
          
          doce_pen <- pen*(1 + crec_pensiones)^(j-1)*12*axn(TM, x = edad_jubilacion + j -1, n=1, k=12, i = i_actuarial, payment = "immediate")
          dec_ter <- pen*(1 + crec_pensiones)^(j-1)*axn(TM,x = edad_jubilacion + j -1, n = 1, i = i_actuarial, payment = "immediate")
          dec_cua <- sbu_anio_jub*(1 + crec_SBU)^(j-1)*axn(TM,x = edad_jubilacion + j -1,n = 1,i = i_actuarial, payment = "immediate")
          
          ev_res2[j,"gasto_anio"] <- round(doce_pen + dec_ter + dec_cua, 2)
          ev_res2[j,"res_acum"]   <- ev_res2[j-1,"res_acum"]*(1+i_actuarial) - ev_res2[j,"gasto_anio"]
        }
        
      }
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
    
  })
  output$evolucion_reservas_sin_aporte_con_decimos <- renderHighchart({
    
    edad <-  Evolucion_Reservas_sin_aporte40_con_decimos()[[1]]
    reservas <-  Evolucion_Reservas_sin_aporte40_con_decimos()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo",
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text = "evolucion_reservas_sin_aporte_con_decimos") %>% 
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
      ) %>% hc_add_theme(hc_theme_elementary())
  })
          # Después: En conjunto
  output$evolucion_reservas_con_y_sin_aporte_con_decimos <- renderHighchart({
    
    edad <- Evolucion_Reservas_con_aporte40_con_decimos()[[1]]
    reservas_sin <- Evolucion_Reservas_sin_aporte40_con_decimos()[[2]]
    reservas_con <- Evolucion_Reservas_con_aporte40_con_decimos()[[2]]
    
    max_res <- (input$edad_jubilacion - input$edad_inicio)
    last_pos_sin <- max(which(reservas_sin > 0))
    first_neg_sin <- min(which(reservas_sin < 0))
    
    last_pos_con <- max(which(reservas_con > 0))
    first_neg_con <- min(which(reservas_con < 0))
    
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo",
               style = list(fontSize = "14px")) %>%
      hc_subtitle(text = "Evolucion reservas con y sin aporte del Estado, y <b> considerando la reforma </b>",
                  style = list(fontSize = "11px")) %>% 
      hc_xAxis(categories = edad, title = list(text = "Edad")) %>%
      hc_yAxis(title = list(text = "Reservas")) %>%
      hc_tooltip(shared = TRUE, valueDecimals = 0) %>%
      hc_plotOptions(area = list(
        marker = list(enabled = FALSE),
        enableMouseTracking = TRUE
      )) %>%
      hc_add_series(
        name = "Aportación",
        data = reservas_sin[1:max_res],  # datos positivos hasta 150000
        color = "blue",
        fillOpacity = 0.3
      ) %>%
      hc_add_series(
        name = "Jubilación sin considerar aporte estatal",
        data = c(rep(NA, max_res-1), reservas_sin[max_res:last_pos_sin]),  # datos decreciendo de 150000 a 0
        color = "green",
        fillOpacity = 0.3
      ) %>%
      hc_add_series(
        name = "Jubilación (déficit) sin considerar aporte estatal",
        data = c(rep(NA, last_pos_sin-1), reservas_sin[(last_pos_sin):length(reservas_sin)]),  # datos decreciendo de 0 a -150000
        color = "red",
        fillOpacity=0.3
      ) %>%
      hc_add_series(
        name = "Jubilación considerando aporte estatal",
        data = c(rep(NA, max_res-1), reservas_con[max_res:last_pos_con]),  # datos decreciendo de 150000 a 0
        color = "#747474",
        type = "line",
        dashStyle = "dot"
      ) %>%
      hc_add_series(
        name = "Jubilación (déficit) considerando aporte estatal",
        data = c(rep(NA, last_pos_con-1), reservas_con[(last_pos_con):length(reservas_con)]),  # datos decreciendo de 0 a -150000
        color = "#747474",
        type = "line",
        dashStyle = "dot"
      ) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  # Grafico Impacto del porcentaje
  output$deficit_porcentaje_01 <- renderHighchart({
    y <- c()
    for (k in 0:40){
      y <- c(y , fun_anio_deficit(calcularAhorroTotal_01(), calcularVApensionesTotal()[[1]] ,calcularVApensionesTotal()[[2]] , k))
    }
    
    data <- data.frame(
      anio = y,
      porcentaje = seq(0, 40, by = 1)
    )
    
    # Crear gráfico de Highcharts
    data %>% 
      hchart(., 
             type = "line", 
             name= 'Porcentaje de Aportación del Estado',
             hcaes(x = anio, 
                   y = porcentaje)) %>% 
      hc_title(text = "Impacto del Porcentaje de Aportación del Estado en la Sostenibilidad de las Pensiones",
               style = list(fontSize = "14px")) %>% 
      hc_subtitle(text = "Edad del jubilado al agotarse los recursos para el pago de su pensión según el porcentaje de aporte del Estado",
                  style = list(fontSize = "11px")) %>% 
      hc_xAxis(title = list(text = "Edad del jubilado donde se alcanza el déficit")) %>%  
      hc_yAxis(title = list(text = "Porcentaje de Aportación del Estado")) %>% 
      hc_add_theme(hc_theme_elementary()) %>%
      hc_tooltip(pointFormat = 'Edad del jubilado: {point.x}<br>Porcentaje de Aportación del Estado: {point.y}') 
  })
  
  # Cobertura del Estado Ecuatoriano sobre las prestaciones
  output$porc_cobertura_01 <- renderHighchart({
    
    x <- round(((calcularVApensionesTotal()[[1]]- calcularAhorroTotal_01())/calcularVApensionesTotal()[[1]])*100,1)
    if(x < 0){
      x <- 0
    }
    
    porc_estado <- x
    vaa_prestacion <- round(calcularVApensionesTotal()[[1]],1)
    vaa_prestacion_estado <- vaa_prestacion*(porc_estado/100)
    vaa_prestacion_iess <- vaa_prestacion*(1-(porc_estado/100))
    
    colores <- c('#605CA8', '#0073B7')
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Cobertura del Estado Ecuatoriano sobre las prestaciones del Jubilado",
               style = list(fontSize = "14px")) %>%
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = TRUE, format = '{point.percentage:.1f} %'),
        showInLegend = TRUE
      )) %>%
      hc_series(list(
        name = "Dólares",
        colorByPoint = TRUE,
        data = list(
          list(name = "Prestaciones Cubiertas por el IESS", y = vaa_prestacion_iess, color = colores[1]),
          list(name = "Prestaciones Cubiertas por el Estado", y = vaa_prestacion_estado, color = colores[2])
        )
      )) 
  })
  
  # Tabla 
  output$tabla_escenario_01 <- renderText({
    paste("verificar funciones de arriba!!!!!","")
  })
  
  # TABLA -----------
  Ahorro_01 <- function(edad_inicio, inflacion, interes, edad_jubilacion, salario, anio_inicio){
    inflacion <-  inflacion/ 100 
    interes <-  interes / 100
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    salario0 <- salario
    
    if(salario0 < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario0 <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+incremento) , 
                  n = anios_aporte, 
                  i = interes,
                  type = "due")
    Ahorro <- Ahorro + VSn(C = (salario0 * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                             q = (1+incremento) , 
                             n = anios_aporte , #años de aporte hasta el momento
                             i = interes,
                             type = "due")
    return(Ahorro)
  }
  Evolucion_Reservas_fun_01 <- function(sexo, edad_inicio, edad_jubilacion,salario_ini, anio_inicio, interes,aporte = TRUE){
    # Parámetros  
    anio_fin <- anio_inicio + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    i_actuarial <- interes /100 # 6.2500 /100 #tasa actuarial
    i_12 <- (1+i_actuarial)^(1/12) - 1
    
    # Crecimiento de la reserva ( cotizaciones ) -----------------------------------
    ev_res <- data.frame(
      anio = c(anio_inicio:anio_fin),
      edad = c(edad_inicio:(edad_jubilacion-1)),
      res_acum = numeric(num_anios)
    )
    if(salario_ini < sbu$sbu[sbu$anio==input$anio_inicio]){
      salario_ini <- sbu$sbu[sbu$anio==input$anio_inicio]
      incremento <-2.5339/100
    }else{
      incremento <- 0.02154
    }
    for (j in c(1:num_anios)) {
      ev_res[j,3] <-  VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=12, type = "immediate"),
                          q = (1+incremento) , 
                          n = j , #años de aporte hasta el momento
                          i = i_actuarial,
                          type = "due")
      # Aportación extra del décimo tercer y décimo cuarto sueldo
      ev_res[j,3] <- ev_res[j,3] + VSn(C = (salario_ini * IVM) * annuity(i = i_12, n=2, type = "immediate"),
                                       q = (1+incremento) , 
                                       n = j , #años de aporte hasta el momento
                                       i = i_actuarial,
                                       type = "due")
    }
    
    # Decrecimiento de la reserva ( pensiones ) -------------------------------------
    
    pen <- Pension(edad_inicio, salario_ini, num_anios)[[1]]*(1+crec_pensiones)^(input$edad_jubilacion - (input$edad_inicio + 2024 - input$anio_inicio)) #ammy_estuvo_aqui
    ahorro_total_inicial <- ev_res[nrow(ev_res),3]
    
    if(aporte == TRUE){
      # con el 40% del estado
      ev_res2 <- data.frame(
        anio = c((anio_fin+1):anio_muerte),
        edad = c(edad_jubilacion:100),
        res_acum = numeric(num_anios_pension),
        gasto_anio = numeric(num_anios_pension)
      )
      
      x=edad_jubilacion
      m=12
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
    }else{
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
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
  }
  
  output$tabla_pensiones_01 <- renderReactable({
    # Creación de las columnas
    edad_jubilacion <- seq(input$edad_jubilacion, input$edad_jubilacion + 10)
    anos_aportacion <- edad_jubilacion - input$edad_inicio
    fondo_acumulado <- sapply(edad_jubilacion, function(edad) {
      Ahorro_01(input$edad_inicio, input$inflacion, input$interes, edad, input$salario, input$anio_inicio)
    })
    fondo_acumulado <- format(fondo_acumulado, big.mark = ",", decimal.mark = ".", nsmall = 2)
    fondo_acumulado <- paste("USD", fondo_acumulado)
    
    ultimo_anio_reserva_con_aporte <- sapply(edad_jubilacion, function(edad_jub) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun_01(input$sexo, input$edad_inicio,
                                                           edad_jub, input$salario,
                                                           input$anio_inicio, input$interes,
                                                           aporte = TRUE)[[2]] > 0)) - 2
    })
    ultimo_anio_reserva_sin_aporte <- sapply(edad_jubilacion, function(edad_jub) {
      input$edad_inicio + max(which(Evolucion_Reservas_fun_01(input$sexo, input$edad_inicio,
                                                           edad_jub, input$salario,
                                                           input$anio_inicio, input$interes,
                                                           aporte = FALSE)[[2]] > 0)) - 2
    })
    pension_anual <- sapply(edad_jubilacion, function(edad) {
      pension_base <- Pension(input$edad_inicio, input$salario, (edad - input$edad_inicio))[[1]] * 12
    })
    pension_mensual <- pension_anual / 12
    pension_anual <- format(pension_anual, big.mark = ",", decimal.mark = ".", nsmall = 2)
    pension_anual <- paste("USD", pension_anual)
    pension_mensual <- format(pension_mensual, big.mark = ",", decimal.mark = ".", nsmall = 2)
    pension_mensual <- paste("USD", pension_mensual)
    
    tasa_reemplazo <- sapply(edad_jubilacion, function(edad) {
      round(TasaReemplazo(input$edad_inicio, input$salario, (edad - input$edad_inicio)), 2)
    })
    
    # Creación de la tabla final
    tabla <- data.frame(
      `Edad de jubilación` = edad_jubilacion,
      `Años de aportación` = anos_aportacion,
      `Fondo acumulado` = fondo_acumulado,
      `Pensión de jubilación anual` = pension_anual,
      `Pensión de jubilación mensual` = pension_mensual,
      `Último año con reserva positiva con aporte del estado` = ultimo_anio_reserva_con_aporte,
      `Último año con reserva positiva sin aporte del estado` = ultimo_anio_reserva_sin_aporte,
      `Tasa de Reemplazo` = tasa_reemplazo
    )
    
    colnames(tabla) <- c("Edad de Jubilación",
                         "Años de aportación",
                         "Fondo acumulado",
                         "Pensión de jubilación anual",
                         "Pensión de jubilación mensual",
                         "Último año con reserva positiva con aporte del estado",
                         "Último año con reserva positiva sin aporte del estado",
                         "Tasa de Reemplazo")
    
    # Convertir la tasa de reemplazo en una barra de progreso
    tabla$`Tasa de Reemplazo` <- sapply(tabla$`Tasa de Reemplazo`, function(x) {
      percent <- as.numeric(x) / 100
      formatted_percent <- sprintf("%.2f", x)
      bar <- paste0('<div style="background-color: #f2f2f2; border-radius: 5px; width: 100px; display: inline-block;">',
                    '<div style="background-color: #4CAF50; width: ', percent * 100, '%; height: 15px; border-radius: 5px;"></div>',
                    '</div> ', formatted_percent, '%')
      bar
    })
    
    # Creación de la tabla con reactable
    reactable(
      tabla,
      columns = list(
        `Edad de Jubilación` = colDef(align = "center"),
        `Años de aportación` = colDef(align = "center"),
        `Fondo acumulado` = colDef(align = "center", style = list(color = "green", fontWeight = "bold")),
        `Pensión de jubilación anual` = colDef(align = "center"),
        `Pensión de jubilación mensual` = colDef(align = "center", style = list(color = "#110C87", fontWeight = "bold")),
        `Último año con reserva positiva con aporte del estado` = colDef(align = "center"),
        `Último año con reserva positiva sin aporte del estado` = colDef(align = "center"),
        `Tasa de Reemplazo` = colDef(html = TRUE, align = "center", minWidth = 110, headerStyle = list(textAlign = "center"))
      ),
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 6,
      theme = reactableTheme(
        headerStyle = list(backgroundColor = "#0073B7", color = "white", fontWeight = "bold"),
        borderColor = "#d9d9d9"
      )
    )
  })

  ### Escenario 2 ----
  
  output$pen1 <- renderText({
    paste("funcion pension:",Pension(input$edad_inicio, input$salario, input$edad_jubilacion - input$edad_inicio)[[1]] )
  })
  output$pen2 <- renderText({
    paste("funcion pension actual:",PensionActual(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), input$edad_jubilacion - input$edad_inicio)[[1]] )
  })
  output$pen3 <- renderText({
    paste("funcion pension actual:", calcularVApensionesTotal()[[2]] )
  })
  output$pen4 <- renderText({
    paste("pension Taynita para calcular VA pensiones :", calcularVApensionesTotal()[[2]] )
  })
  
}

