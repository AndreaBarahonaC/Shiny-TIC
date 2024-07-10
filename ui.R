library(shiny)
library(lubridate)
library(lifecontingencies)
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)
library(shinythemes)
library(highcharter)
library(shinydashboard)
library(DT)
library(kableExtra)
library(ggplot2)



# cerulean, cosmo, cyborg, darkly, flatly, journal, 
# lumen, paper, readable, sandstone, simplex, slate, 
# spacelab, superhero, united, yeti.

ui <- fluidPage(
  theme = shinytheme("paper"),
  tags$head(
    tags$style(HTML("
      .box {
        border: 0.5px solid #3e3f3a; /* Bordes de las cajas */
        padding: 5px; /* Espaciado interno */
        margin-bottom: 5px; /* Espaciado entre cajas */
        border-radius: 5px; /* Bordes redondeados */
      }
      .box-title {
        font-size: 2px; /* Tamaño del título */
        font-weight: bold; /* Negrita para el título */
      }
    "))
  ),
  
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
    "Análisis General",
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
                 "Cálculo de Ahorro",
                 fluidRow(
                   column(6,
                          highchartOutput("deficit_porcentaje", height = "300px"),
                          highchartOutput("evolucion_reservas", height = "300px"),
                          fluidRow(
                            column(6, highchartOutput("penprom_vs_penind", height = "300px")),
                            column(6,  highchartOutput("ahorros_vs_prestaciones", height = "300px"))
                          )
                   ),
                   column(6,
                          fluidRow(
                            column(6, highchartOutput("tiempo_sss", height = "300px")),
                            column(6, highchartOutput("porc_cobertura", height = "300px"))
                          ),
                          tableOutput("tabla_pensiones"),
                   )
                 ),
                 fluidRow(
                   column(4,
                          box(title = tags$h4("Ahorro hasta el momento de la jubilación"), status = "primary", solidHeader = TRUE, width = 12,
                              textOutput("ahorro")),
                          box(title = tags$h4("Cobertura del Estado"), status = "danger", solidHeader = TRUE, width = 12,
                              textOutput("cobertura")),
                          box(title = tags$h4("Tasa de Reemplazo"), status = "warning", solidHeader = TRUE, width = 12,
                              textOutput("tasa_reemplazo"))
                   ),
                   column(4,
                          box(title = tags$h4("Años Aportados"), status = "info", solidHeader = TRUE, width = 12,
                              textOutput("Naportes")),
                          box(title = tags$h4("Pensión Teórica Actual"), status = "primary", solidHeader = TRUE, width = 12,
                              textOutput("pension_teorica_actual")),
                          box(title = tags$h4("Pensión Teórica a la Jubilación"), status = "info", solidHeader = TRUE, width = 12,
                              textOutput("pension_teorica_jub"))
                   ),
                   column(4,
                          box(title = tags$h4("Pensión Promedio Actual"), status = "success", solidHeader = TRUE, width = 12,
                              textOutput("pensionpromedio")),
                          box(title = tags$h4("Valor de la Pensión al momento de la jubilación"), status = "warning", solidHeader = TRUE, width = 12,
                              textOutput("VApension")),
                          box(title = tags$h4("Pensión Teórica a la Jubilación (Opción 2)"), status = "success", solidHeader = TRUE, width = 12,
                              textOutput("pension_teorica_jub_o"))
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
               )
             )
      )
    )
  )
)
