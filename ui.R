library(shiny)

ui <- fluidPage(
  titlePanel("Cálculo de Ahorro"),
  
  sidebarLayout(
    sidebarPanel(
      # Se usan como valores determinados los especificados por el IESS
      # Tasa actuarial = 6.25%
      # Tasa de variación de la inflación 1.826%
      
      radioButtons("sexo", "Seleccione su sexo:",
                   c("Hombre" = "M", "Mujer" = "F"), selected = "F"),
      numericInput("edad_inicio", "Edad de inicio de cotizaciones (años):", value = 25, min = 18, max= 100),
      numericInput("interes", "Interés (%):", value = 6.25, min = 0),
      numericInput("inflacion", "Inflación (%):", value = 1.826, min = 0),
      numericInput("salario", "Primer sueldo al iniciar las cotizaciones:", value = 850, min = 0),
      numericInput("edad_jubilacion", "Edad de jubilación (años):", value = 65, min = 65, max=100),
      textOutput("minjub")
    ),
    
    mainPanel(
      textOutput("ahorro"), 
      textOutput("Naportes"),
      textOutput("VApension"),
      textOutput("cobertura")
    )
  )
)