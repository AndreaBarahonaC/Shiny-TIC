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

server <- function(input, output) {
  calcularAhorro <- reactive({
    edad_inicio <- input$edad_inicio
    salario0 <- input$salario
    inflacion <- input$inflacion / 100 
    interes <- input$interes /100
    edad_jubilacion <- input$edad_jubilacion
    anios_aporte <- edad_jubilacion-edad_inicio
    
    # Calculo del ahorro de un afiliado hasta su jubilación
    i_12 <- (1+interes)^(1/12) - 1
    
    Ahorro <- VSn(C = (salario0 * IVM) * annuity(i = i_12, n=12, type = "due"),
                  q = (1+inflacion) , 
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
  
}