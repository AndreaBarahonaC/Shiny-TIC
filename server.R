library(shiny)
library(lifecontingencies)

# Progresión Geometrica ------------------------
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



#  Se usa por defecto la siguiente información obtenida del IESS
# Aporte del afiliado (personal y patronal) al seguro= 11,06% de su salario
# Tasa de crecimiento de salarios= 2.154%

server <- function(input, output, session) {
  
  # Verificar mínimo de edad de jubilación
  observeEvent(input$edad_inicio, {
    # Verificar si el valor de edad_inicio no es NULL ni NA
    if (!is.null(input$edad_inicio) && !is.na(input$edad_inicio)) {
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
      
      updateNumericInput(session, "edad_jubilacion", min = minimo(input$edad_inicio))
    }
  })
  
 
  ###
  calcularAhorro <- reactive({
    edad_inicio <- input$edad_inicio
    salario0 <- input$salario
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    edad_jubilacion <- input$edad_jubilacion
    anios_aporte <- edad_jubilacion-edad_inicio
    
    IVM <- 0.1106 
    crec_salarios <- 0.02154
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+inflacion)*(1+crec_salarios) , 
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
  
  #Función de cálculo de la pensión promedio
  
  pension_promedio_fun <- function(e_j, impo, sexo1){
    res <- pensiones2 %>%  dplyr::filter(edad_jubilacion == e_j) %>%  
      dplyr::filter(numero_imposiciones_totales == impo) %>% 
      dplyr::filter(sexo == sexo1)
    #prom <- mean(res$valor_pension_concedida) 
    prom <- mean(res$pension_final)
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
  
  #Cálculo de la pensión promedio
  
  pension_promedio <- pension_promedio_fun(edad_jubilacion, anios_aporte*12, sexo)
  
  # Calculo del VA de la pension
  i_12 <- (1+interes)^(1/12) - 1
  
  C <- (pension_promedio *(1+ crec_pensiones)^anios_aporte* (1+inflacion)^anios_aporte) * annuity(i = i_12, n=12, type = "due")
  if(sexo == 'M'){
    va_pension <- C * axn(TH, x= edad_jubilacion, n=n_pensiones, i= (interes-inflacion-crec_pensiones- crec_pensiones*inflacion)/((1+inflacion)*(1+crec_pensiones)), payment='due')
  }else{
    va_pension <- C * axn(TM, x= edad_jubilacion, n=n_pensiones, i= (interes-inflacion-crec_pensiones- crec_pensiones*inflacion)/((1+inflacion)*(1+crec_pensiones)), payment='due')
    
  }
  
  return(va_pension)
  
})


output$VApension <- renderText({
  paste("El valor actual actuarial de la pensión a otorgarse es: ", round(calcularVApensiones(),1))
})

output$cobertura <- renderText({
  paste("Porcentaje con el que debe aportar el Estado Ecuatoriano para cubrir el pago de la pensión del individuo: ", 
        round(((calcularVApensiones()- calcularAhorro())/calcularVApensiones())*100,1), '%')
})





}










