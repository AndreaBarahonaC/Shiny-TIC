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
library(highcharter)
library(DT)
library(kableExtra)
library(ggplot2)
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
      paste("El ahorro es: $", round(calcularAhorro(),1))
    })
  
  output$Naportes <- renderText({
    paste("Aportó:", input$edad_jubilacion - input$edad_inicio, "años.")
  })
  
  
  # Carga de las tablas de mortalidad ----
  
  probsH<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                    sheet=1)[,c(3)]))
  probsM<- unname(unlist(read_excel("Probabilidades_Ecuador_2023_2060.xlsx",
                                    sheet=2)[,c(3)]))
  
  ls("package:lifecontingencies")
  TH <- probs2lifetable(probs=probsH, radix=100000, type="qx", name = "Mortalidad Hombres")
  TM <- probs2lifetable(probs=probsM, radix=100000, type="qx", name = "Mortalidad Mujeres")
  
  



  # Cálculo del valor actual de las prestaciones a otorgarse ----

  # Tasa de crecimiento del SBU= 2.5339%
  # Tasa de crecimiento de pensiones= 1.8261%
  
  # load("C:/Users/MyHP/Desktop/TIC/Shiny-TIC/obtencion_pension_prom.Rdata") #Taynita
  load("obtencion_pension_prom.Rdata")
  
  # arreglo base de datos 
  # establecimiento minimo y maximo de pensiones
  
  pensiones2 <- pensiones2 %>%  mutate( prom_salario_a_usar = promedio_sueldo_real *(1.02154)^(2024 - as.numeric(format(as.Date(fecha_inicial_pension, '%Y %m %d'), "%Y"))), .before=promedio_sueldo_real)
  pensiones2 <- pensiones2 %>%  mutate(pension_final1 = pension_final *( 1.018261)^(2024 - as.numeric(format(as.Date(fecha_inicial_pension, '%Y %m %d'), "%Y"))), .before=pension_final)
  
  pensiones2 <- pensiones2 %>% mutate(pensionesfinal2 = pension_final1)
  pensiones2 <- pensiones2 %>% dplyr::filter(tipo_seguro != 'SC')
  
  # Función de cálculo de la pensión promedio -----
  
  pension_promedio_fun <- function(e_j, impo, sexo1, sal_ini, anios_aporte){
    crec_salarios <- 0.02154
    
    res <- pensiones2 %>%  dplyr::filter(minimo(input$edad_inicio) <=edad_jubilacion & edad_jubilacion <= e_j + 3) %>%  
      dplyr::filter(impo + 24 >= numero_imposiciones &  numero_imposiciones >= ((minimo(input$edad_inicio) - input$edad_inicio)*12) ) %>% 
      dplyr::filter(sexo == sexo1) %>% 
      mutate(prom_salario_a_usar1 = ifelse(prom_salario_a_usar ==0, mean(pensiones2$prom_salario_a_usar[pensiones2$prom_salario_a_usar !=0]),prom_salario_a_usar)) %>% 
      dplyr::filter( Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]] - 300 <= prom_salario_a_usar1 & prom_salario_a_usar1 <= Pension(input$edad_inicio, sal_ini, anios_aporte)[[2]]+100)
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
    
    pension_promedio <- Pension(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio))[[1]]
    #pension_promedio <- pension_promedio_fun(edad_jubilacion, anios_aporte*12, sexo, salario, anios_aporte)
    
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
    paste("El valor actual actuarial de la pensión a otorgarse es: $", round(calcularVApensiones()[[1]],1))
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
          round( TasaReemplazo(input$edad_inicio, input$salario, (input$edad_jubilacion - input$edad_inicio)), 2),"%"
    )
  })
  
  # Evolucion de la reserva del individuo
  
  Evolucion_Reservas <- reactive({

  # Parámetros  ---------------------------------------------------
    
    sexo <- input$sexo
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- ifelse(input$edad_jubilacion>=60,input$edad_jubilacion,60)
    salario_ini <- input$salario
    
    
    crec_pensiones <- 1.8261/100; crec_pensiones_12 <- (1+crec_pensiones)^(1/12)-1
    
    interes <- input$interes /100 # 6.2500 /100 #tasa actuarial
    
    crec_salarios <- 0.02154; crec_salarios_12 <- (1+crec_salarios)^(1/12)-1 #superiodal
    
    IVM <- 11.06/100
    anio_ini <- input$anio_inicio
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
      if(sexo == "M"){
        ev_res2[j,3] <-  ev_res[nrow(ev_res),3] - pen*12*axn(TH,x=x,n=n,i=i,payment = "due")- ((m-1)/(2*m))*(1-Exn(TH,x=x,n=n,i=i))
      }else{
        ev_res2[j,3] <-  ev_res[nrow(ev_res),3] - pen*12*axn(TM,x=x,n=n,i=i,payment = "due")- ((m-1)/(2*m))*(1-Exn(TM,x=x,n=n,i=i))
      }
      
      
    }
    
    edad <- c(ev_res[,2],ev_res2[,2])
    reservas <- c(ev_res[,3],ev_res2[,3])
    
    return(list(edad, reservas))
    
  })

  output$evolucion_reservas <- renderHighchart({
  
    edad <- Evolucion_Reservas()[[1]]
    reservas <- Evolucion_Reservas()[[2]]
    
    max_res <- which(reservas==max(reservas))
    last_pos <- max(which(reservas > 0))
    first_neg <- min(which(reservas < 0))
    
  
    highchart() %>%
      hc_chart(type = "area") %>%
      hc_title(text = "Evolución del Ahorro y Gasto por edad del Individuo") %>%
      hc_subtitle(text = "Ahorro acumulado del cotizante, gasto y déficit en las prestaciones en cada edad del jubilado.") %>% 
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
  
  # GRAFICO, CUÁNDO SE PRESENTA EL DÉFICIT DEPENDIENDO DEL APORTE DEL ESTADO DEL 10% AL 40%
  # USO DE LA INFORMACIÓN DADA POR LA BASE DE DATOS PROPORCIONADA

  fun_anio_deficit<- function (ahorro, va_pensiones, pension_inicial, porcentaje_estado){

      ahorro <- ahorro + (porcentaje_estado/100)* va_pensiones
      crec_pensiones <- 1.8261/100
      interes <- input$interes/100
      i_12 <- (1+(input$interes/100))^(1/12) - 1

      pension <- pension_inicial * annuity(i = i_12, n=12, type = "due")
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
        y <- c(y , fun_anio_deficit(calcularAhorro(), calcularVApensiones()[[1]] ,calcularVApensiones()[[2]] , k))
      }
  
      data <- data.frame(
        x = seq(0, 40, by = 1),
        y = y
      )
  
      # Crear gráfico de Highcharts
      data %>% 
        hchart(., 
               type = "line",styled = TRUE,
               name= 'Edad del jubilado',
               hcaes(x = x, 
                     y = y)) %>% 
        hc_title(text = "Impacto del Porcentaje de Aportación del Estado en la Sostenibilidad de las Pensiones") %>% 
        hc_subtitle(text = "Edad del jubilado al agotarse los recursos para el pago de su pensión según el porcentaje de aporte del Estado") %>% 
        hc_xAxis(title = list(text = "Porcentaje de Aportación del Estado")) %>%  
        hc_yAxis(title = list(text = "Edad del jubilado donde se alcanza el déficit")) %>% 
        hc_add_theme(hc_theme_elementary()) 
  })


  # Graficos interactivos para primeros resultados
  
  # Tiempo en el Sistema de Seguridad Social
  
  output$tiempo_sss <- renderHighchart({
  
    edad_inicio <- input$edad_inicio
    edad_jubilacion <- ifelse(input$edad_jubilacion>=60,input$edad_jubilacion,60)
    anio_ini <- input$anio_inicio
    anio_fin <- anio_ini + (edad_jubilacion-edad_inicio-1)
    num_anios <- edad_jubilacion - edad_inicio
    anio_muerte <- anio_fin+1 + (100-edad_jubilacion)
    num_anios_pension <- length(c((anio_fin+1):anio_muerte))
    
    highchart() %>%
      hc_chart(type = "pie") %>%
      hc_title(text = "Tiempo del Individuo en el Sistema de Seguridad Social") %>%
      hc_subtitle(text = "Número de años del Individuo como cotizante y como pensionista en el IESS") %>% 
      hc_plotOptions(pie = list(
        allowPointSelect = TRUE,
        cursor = "pointer",
        dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
        showInLegend = TRUE
      )) %>%
      hc_series(list(
        name = "Años",
        colorByPoint = TRUE,
        data = list(
          list(name = "Cotizante", y = num_anios),
          list(name = "Pensionista", y = num_anios_pension)
        )
      )) 
  })

  # Porcentaje de cobertura del estado en prestaciones pension

  output$porc_cobertura <- renderHighchart({
  
  porc_estado <- round(((calcularVApensiones()[[1]]- calcularAhorro())/calcularVApensiones()[[1]])*100,1)
  vaa_prestacion <- round(calcularVApensiones()[[1]],1)
  vaa_prestacion_estado <- vaa_prestacion*(porc_estado/100)
  vaa_prestacion_iess <- vaa_prestacion*(1-(porc_estado/100))
  
  highchart() %>%
    hc_chart(type = "pie") %>%
    hc_title(text = "Cobertura del Estado Ecuatoriano sobre las prestaciones del Jubilado") %>%
    hc_subtitle(text = "Porcentaje aportado por el Estado Ecuatoriano para el pago de pensiones del Jubilado") %>% 
    hc_plotOptions(pie = list(
      allowPointSelect = TRUE,
      cursor = "pointer",
      dataLabels = list(enabled = TRUE, format = '<b>{point.name}</b>: {point.percentage:.1f} %'),
      showInLegend = TRUE
    )) %>%
    hc_series(list(
      name = "Dólares",
      colorByPoint = TRUE,
      data = list(
        list(name = "Prestaciones Cubiertas por el IESS", y = vaa_prestacion_iess),
        list(name = "Prestaciones Cubiertas por el Estado", y = vaa_prestacion_estado)
      )
    )) 
})

  # Comparacion ahorros vs prestaciones

  output$ahorros_vs_prestaciones <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Diferencia entre el ahorro y el valor total de las prestaciones del Individuo") %>%
    hc_xAxis(categories = list('')) %>%
    hc_yAxis(min = 0, title = list(text = "Dólares")) %>%
    hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
    hc_plotOptions(column = list(
      pointPadding = 0.2,
      borderWidth = 0
    )) %>%
    hc_series(list(
      name = "Ahorros totales del Individuo",
      data = list(round(calcularAhorro(),1))
    ), list(
      name = "Valor Actual Actuarial de las prestaciones",
      data = list(round(calcularVApensiones()[[1]],1))
    ))
})


  # Comparacion pension promedio y pension del individuo

  output$penprom_vs_penind <- renderHighchart({
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_title(text = "Comparación entre la Pensión del Individuo y la Pensión Promedio") %>%
      hc_subtitle(text = "Pnsión que obtendrá el Individuo, comparada con la pensión de in individuo de similares características.") %>% 
      hc_xAxis(categories = list('')) %>%
      hc_yAxis(min = 0, title = list(text = "Dólares")) %>%
      hc_tooltip(shared = TRUE, crosshairs = TRUE) %>%
      hc_plotOptions(column = list(
        pointPadding = 0.2,
        borderWidth = 0
      )) %>%
      hc_series(list(
        name = "Pensión Promedio",
        data = list(round(calcularVApensiones()[[2]],1))
      ), list(
        name = "Pensión del Individuo",
        data = list(round(Pension(input$edad_inicio, input$salario *(1+ 0.02154)^(-input$anio_inicio + (2024 - (input$edad_jubilacion - input$edad_inicio))), (input$edad_jubilacion - input$edad_inicio))[[1]], 2))
      ))
  })
  
  # TABLA -----------
  Ahorro <- function(edad_inicio, inflacion, interes, edad_jubilacion, salario, anio_inicio){
    inflacion <-  inflacion/ 100 
    interes <-  interes / 100
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    salario0 <- salario *(1+ crec_salarios)^(- anio_inicio + (2024 - anios_aporte))
    
    
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+crec_salarios) , 
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
    salario <- salario *(1+ crec_salarios)^(-anio_inicio + (2024 - anios_aporte))
    #Cálculo de la pensión promedio
    
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
  
  fun_anio_deficit_2 <- function(interes, ahorro, va_pensiones, pension_inicial, porcentaje_estado){
    ahorro <- ahorro + (porcentaje_estado/100)* va_pensiones
    crec_pensiones <- 1.8261/100
    interes <- interes/100
    i_12 <- (1+(interes/100))^(1/12) - 1
    
    pension <- pension_inicial * annuity(i = i_12, n=12, type = "due")
    i <- 1
    
    while((ahorro - pension) > 0){
      ahorro <- (ahorro - pension)* (1 + interes)
      pension <- pension * (1+ crec_pensiones)
      i <- i + 1
      if (!is.finite(ahorro)) {
        return(110) 
      }
      
    }
    
    return(i)
  }
  
  output$tabla_pensiones <- function(){
    # Creación de las columnas
    edad_jubilacion <- seq(input$edad_jubilacion, input$edad_jubilacion + 10)
    anos_aportacion <- edad_jubilacion - input$edad_inicio
    fondo_acumulado <- sapply(edad_jubilacion, function(edad) {
      Ahorro(input$edad_inicio, input$inflacion, input$interes, edad, input$salario, input$anio_inicio)
    })
    
    ultimo_anio_reserva <- sapply(edad_jubilacion, function(edad) {
      edad + fun_anio_deficit_2(input$interes,
                                Ahorro(input$edad_inicio, input$inflacion, input$interes, edad, input$salario, input$anio_inicio), 
                                VApensiones_fun(input$sexo, input$edad_inicio, edad, input$inflacion, input$interes, input$salario, input$anio_inicio)[[1]],
                                VApensiones_fun(input$sexo, input$edad_inicio, edad, input$inflacion, input$interes, input$salario, input$anio_inicio)[[2]], 
                                ((VApensiones_fun(input$sexo, input$edad_inicio, edad, input$inflacion, input$interes, input$salario, input$anio_inicio)[[1]] - Ahorro(input$edad_inicio, input$inflacion, input$interes, edad, input$salario, input$anio_inicio))/VApensiones_fun(input$sexo, input$edad_inicio, edad, input$inflacion, input$interes, input$salario, input$anio_inicio)[[1]])*100
      )
    })
    pension_anual <- sapply(edad_jubilacion, function(edad) {
      pension_base <- Pension(input$edad_inicio, input$salario , (edad - input$edad_inicio))[[1]] * 12
    })
    pension_mensual <- pension_anual / 12
    
    # Creación de la tabla final
    tabla <- data.frame(
      `Edad de jubilación` = edad_jubilacion,
      `Años de aportación` = anos_aportacion,
      `Fondo acumulado` = fondo_acumulado,
      `Último año con reserva positiva` = ultimo_anio_reserva,
      `Pensión de jubilación anual` = pension_anual,
      `Pensión de jubilación mensual` = pension_mensual
    )
    
    colnames(tabla) <- c("Edad de Jubilación",
                         'Años de aportación',
                         'Fondo acumulado',
                         'Último año con reserva positiva',
                         'Pensión de jubilación anual',
                         'Pensión de jubilación mensual')
    
    # Generación de la tabla con kableExtra
    kbl(tabla) %>%
      kable_styling(bootstrap_options = "hover",position = "center") %>%
      row_spec(0, bold = TRUE,color="white", background = "#3e3f3a") %>%
      scroll_box(width = "900px", height = "400px")
    
  }

}









