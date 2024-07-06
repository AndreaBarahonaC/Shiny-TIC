library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("readable"),
  # shinythemes::themeSelector(), #tenemos que elegir solo un tema al final
  fluidRow(
    column(3, img(src = "Imagenes/logo_epn.png", height = "100px", align = "right")),
    column(6, align = "center",
           h2("Escuela Politécnica Nacional"),
           h4("Trabajo de Integración Curricular"),
           h5("Evaluación de las propuestas de Reforma al Sistema de Pensiones Ecuatoriano"),
           h5("Profesor: Diego Paúl Huaraca Shagñay"),
           h5("Estudiantes: Andrea Barahona, Tayna Ruiz, Ammy Párraga")
    ),
    column(3, img(src = "Imagenes/logo_fc.png", height = "100px"))
  ),
  
  br(),
  
  navbarPage(
    title = "Título",
    
    tabPanel(
      "Cálculo de Ahorro",
      sidebarLayout(
        sidebarPanel(
          # Se usan como valores determinados los especificados por el IESS
          # Tasa actuarial = 6.25%
          # Tasa de variación de la inflación 1.826%
          radioButtons("sexo", "Seleccione su sexo:",
                       c("Hombre" = "M", "Mujer" = "F"), selected = "F"),
          numericInput("edad_inicio", "Edad de inicio de cotizaciones (años):", value = 25, min = 18, max = 100),
          numericInput("anio_inicio", "Año de inicio de cotizaciones :", value = 2010, min = 1960, max = 2080),
          numericInput("interes", "Interés (%):", value = 6.25, min = 0),
          numericInput("inflacion", "Inflación (%):", value = 1.826, min = 0),
          numericInput("salario", "Primer sueldo al iniciar las cotizaciones:", value = 600, min = 0),
          numericInput("edad_jubilacion", "Edad de jubilación (años):", value = 60, min = 60, max = 100),
          textOutput("minjub")
        ),
        mainPanel(
          textOutput("ahorro"),
          textOutput("Naportes"),
          textOutput("pensionpromedio"),
          textOutput("VApension"),
          textOutput("cobertura"),
          textOutput("pension_teorica_actual"),
          textOutput("pension_teorica_jub"),
          textOutput("pension_teorica_jub_o"),
          textOutput("tasa_reemplazo")
        )
      )
    ),
    
    tabPanel(
      "Análisis Andrea Barahona",
      fluidPage(
        h3("Explicación Ley"),
        p("Aquí puedes agregar más funcionalidades o información adicional.")
      )
    ),
    
    tabPanel(
      "Análisis Tayna Ruiz",
      fluidPage(
        h3("Explicación Ley"),
        p("Aquí puedes agregar más funcionalidades o información adicional.")
      )
    ),
    
    tabPanel(
      "Análisis Ammy Párraga",
      fluidPage(
        h3("Explicación Ley"),
        p("Aquí puedes agregar más funcionalidades o información adicional.")
      )
      
      # theme = bs_theme(preset = "minty")
    )
  )
)