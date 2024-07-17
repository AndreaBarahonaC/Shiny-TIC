suppressMessages(suppressWarnings(library(shiny)))
suppressMessages(suppressWarnings(library(lubridate)))
suppressMessages(suppressWarnings(library(lifecontingencies)))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(library(readxl)))
suppressMessages(suppressWarnings(library(tidyverse)))
suppressMessages(suppressWarnings(library(data.table)))
suppressMessages(suppressWarnings(library(shinythemes)))
suppressMessages(suppressWarnings(library(shinydashboard)))
suppressMessages(suppressWarnings(library(shinyWidgets)))
suppressMessages(suppressWarnings(library(highcharter)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(kableExtra)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(scales)))
suppressMessages(suppressWarnings(library(reactable)))


# cerulean, cosmo, cyborg, darkly, flatly, journal, 
# lumen, paper, readable, sandstone, simplex, slate, 
# spacelab, superhero, united, yeti.

ui <- fluidPage(
  useShinydashboard(),
  theme = shinytheme("paper"),
  
  fluidRow(
    column(3, img(src = "logo_epn.png", height = "100px", align = "right")),
    column(6, align = "center",
           h4("Escuela Politécnica Nacional - Trabajo de Integración Curricular"),
           h6("Evaluación de las propuestas de Reforma al Sistema de Pensiones Ecuatoriano"),
           h6("Profesor: Diego Paúl Huaraca Shagñay"),
           h6("Estudiantes: Andrea Barahona, Tayna Ruiz, Ammy Párraga")
    ),
    column(3, img(src = "logo_fc.png", height = "100px"))
  ),
  
  br(),
  
  tabPanel(
    "TIC",
    fluidRow(
      column(2,
             wellPanel(
               radioButtons("sexo", "Seleccione su sexo:",
                            c("Hombre" = "M", "Mujer" = "F"), selected = "F"),
               numericInput("edad_inicio", "Edad de inicio de cotizaciones (años):", value = 25, min = 18, max = 100),
               numericInput("anio_inicio", "Año de inicio de cotizaciones :", value = 2010, min = 1960, max = 2080),
               numericInput("interes", "Interés (%):", value = 6.25, min = 0),
               numericInput("inflacion", "Inflación (%):", value = 1.826, min = 0),
               numericInput("salario", "Primer sueldo al iniciar las cotizaciones:", value = 600, min = 0),
               numericInput("edad_jubilacion", "Edad de jubilación (años):", value = 60, min = 60, max = 100),
               textOutput("minjub")
             )),
      column(10, 
             tabsetPanel(
               tabPanel(
                 "Sistema Actual",
                 fluidRow(
                   infoBoxOutput("ahorro", width = 4),
                   infoBoxOutput("pension_teorica_actual", width = 4),
                   infoBoxOutput("tasa_reemplazo", width = 4)
                   
                 ),
                 fluidRow(
                   infoBoxOutput('VApension', width = 4),
                   infoBoxOutput("pensionpromedio", width = 4),
                   infoBoxOutput("cobertura", width = 4)
                 ),
                 fluidRow(
                   column(6,
                          highchartOutput("evolucion_reservas_con_aporte", height = "300px"),
                          highchartOutput("deficit_porcentaje", height = "300px"),
                   ),
                   column(6,
                          fluidRow(
                            highchartOutput("tiempo_sss", height = "300px") 
                          ),
                          fluidRow(
                            highchartOutput("porc_cobertura", height = "300px")
                          )
                   )
                 ),
                 fluidRow(
                   reactableOutput("tabla_pensiones")
                 )
               ),
               
               
               tabPanel(
                 "Reforma Andrea Barahona",
                 fluidPage(
                   h4("Reforma: Cálculo en la fórmula de cálculo de la pensión por vejez"),
                   p("Según el Art 199 del Cap. 6 del Anteproyecto de Ley, a partir de la fecha de publicación de la Ley de Pensiones y Ahorro para la 
                     Vejez que reforma la presente Ley de Seguridad Social, la cuantía para el cálculo de la pensión de jubilación 
                     se basará en el promedio mensual de los seis mejores años de ingresos que el afiliado haya aportado, y se incrementará gradualmente
                     a razón de un año por cada año posterior a la reforma hasta alzcanzar un promedio mensual de los 30 mejores años."),
                   fluidRow(
                     sliderInput("anios_calculo_pension", "Número de años considerados para el cálculo de la pensión (años):", value = 8, max = 30, min = 5, width = "35%")
                   ),
                   fluidRow(infoBoxOutput("tasa_reemplazo_reforma", width = 3),  
                            infoBoxOutput("cobertura_reforma", width = 3),
                            infoBoxOutput("pension_teorica_actual_reforma", width = 3),
                            infoBoxOutput('VApension_reforma', width = 3)
                   ),
                   fluidRow(column(6,
                                   #highchartOutput("evolucion_reservas_con_aporte_sin_reforma", height = "300px"),
                                   highchartOutput("evolucion_reservas_con_aporte_con_reforma", height = "300px")
                   ),
                   column(6,
                          highchartOutput("pension_vs_años_con_reforma", height = "300px")
                   )
                   ), 
                   fluidRow(
                     reactableOutput("tabla_pensiones_reformaABC")
                   )
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
               )
             )
      )
    )
  )
)
