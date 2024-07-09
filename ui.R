#prueba
#######


##########
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


ui <- fluidPage(
  theme = shinytheme("readable"),
  tags$head(
    tags$style(HTML("
      .box {
        border: 0.5px solid #FFBCAE; /* Bordes de las cajas */
        padding: 10px; /* Espaciado interno */
        margin-bottom: 15px; /* Espaciado entre cajas */
        border-radius: 10px; /* Bordes redondeados */
      }
      .box-title {
        font-size: 20px; /* Tamaño del título */
        font-weight: bold; /* Negrita para el título */
      }
    "))
  ),
  # shinythemes::themeSelector(), #tenemos que elegir solo un tema al final
  fluidRow(
    column(3, img(src = "logo_epn.png", height = "100px", align = "right")),
    column(6, align = "center",
           h2("Escuela Politécnica Nacional"),
           h4("Trabajo de Integración Curricular"),
           h5("Evaluación de las propuestas de Reforma al Sistema de Pensiones Ecuatoriano"),
           h5("Profesor: Diego Paúl Huaraca Shagñay"),
           h5("Estudiantes: Andrea Barahona, Tayna Ruiz, Ammy Párraga")
    ),
    column(3, img(src = "logo_fc.png", height = "100px"))
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
          fluidRow(
            box(title = tags$h4("Ahorro"), status = "primary", solidHeader = TRUE, width = 6,
                textOutput("ahorro")),
            box(title = tags$h4("Años Aportados"), status = "info", solidHeader = TRUE, width = 6,
                textOutput("Naportes"))
          ),
          fluidRow(
            box(title = tags$h4("Pensión Promedio"), status = "success", solidHeader = TRUE, width = 6,
                textOutput("pensionpromedio")),
            box(title = tags$h4("Valor Actual de la Pensión"), status = "warning", solidHeader = TRUE, width = 6,
                textOutput("VApension"))
          ),
          fluidRow(
            box(title = tags$h4("Cobertura del Estado"), status = "danger", solidHeader = TRUE, width = 6,
                textOutput("cobertura")),
            box(title = tags$h4("Pensión Teórica Actual"), status = "primary", solidHeader = TRUE, width = 6,
                textOutput("pension_teorica_actual"))
          ),
          fluidRow(
            box(title = tags$h4("Pensión Teórica a la Jubilación"), status = "info", solidHeader = TRUE, width = 6,
                textOutput("pension_teorica_jub")),
            box(title = tags$h4("Pensión Teórica a la Jubilación (Opción 2)"), status = "success", solidHeader = TRUE, width = 6,
                textOutput("pension_teorica_jub_o"))
          ),
          fluidRow(
            box(title = tags$h4("Tasa de Reemplazo"), status = "warning", solidHeader = TRUE, width = 12,
                textOutput("tasa_reemplazo"))
          ),
          
          fluidRow(
            box(title = tags$h3("Evolución del ahorro y gasto del individuo"), status = "warning", solidHeader = TRUE, width = 12),
            p("no se me imprime mi tablita :c")
            #,
                # tableOutput("table1"))
          ),
          
          highchartOutput("deficit_porcentaje")
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