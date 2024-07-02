library(shiny)

ui <- fluidPage(
  titlePanel("Cálculo de Ahorro"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("sexo", "Seleccione su sexo:",
                   c("Hombre" = "H", "Mujer" = "M"), selected = "M"),
      numericInput("edad_inicio", "Edad de inicio de cotizaciones (años):", value = 25, min = 18),
      numericInput("interes", "Interés (%):", value = 4, min = 0),
      numericInput("inflacion", "Inflación (%):", value = 3, min = 0),
      numericInput("salario", "Primer sueldo al iniciar las cotizaciones:", value = 850, min = 0),
      numericInput("edad_jubilacion", "Edad de jubilación (años):", value = 65, min = 65)
    ),
    
    mainPanel(
      textOutput("ahorro"), 
      textOutput("Naportes")
    )
  )
)