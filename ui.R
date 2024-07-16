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
                   infoBoxOutput("ahorro", width = 3),
                   infoBoxOutput("Naportes", width = 3),
                   infoBoxOutput("tasa_reemplazo", width = 3),
                   infoBoxOutput("cobertura", width = 3)
                 ),
                 fluidRow(
                   infoBoxOutput('VApension', width = 3),
                   infoBoxOutput("pensionpromedio", width = 3),
                   infoBoxOutput("pension_teorica_actual", width = 3),
                   infoBoxOutput("pension_teorica_jub", width = 3)
                 ),
                 fluidRow(
                   column(6,
                          highchartOutput("evolucion_reservas_con_aporte", height = "300px"),
                          highchartOutput("deficit_porcentaje", height = "300px"),
                   ),
                   column(6,
                          fluidRow(
                            column(6, highchartOutput("tiempo_sss", height = "300px")),
                            column(6, highchartOutput("porc_cobertura", height = "300px"))
                          ),
                          fluidRow(
                            column(6, highchartOutput("penprom_vs_penind", height = "300px")),
                            column(6,  highchartOutput("ahorros_vs_prestaciones", height = "300px"))
                          )
                   )
                 ),
                 fluidRow(
                   tableOutput("tabla_pensiones")
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
                     sliderInput("anios_calculo_pension", "Número de años considerados para el cálculo de la pensión (años):", value = 15, max = 30, min = 5)
                   ),
                   fluidRow(infoBoxOutput("tasa_reemplazo_reforma", width = 3),  
                            infoBoxOutput("pension_teorica_actual_reforma", width = 3),
                            infoBoxOutput("pension_teorica_jub_reforma", width = 3),
                            infoBoxOutput('VApension_reforma', width = 3)
                   ),
                   fluidRow(column(6,
                                   tableOutput("tabla_pensiones_reformaABC")
                   ),
                   column(6,
                          #highchartOutput("evolucion_reservas_con_aporte_sin_reforma", height = "300px"),
                          highchartOutput("evolucion_reservas_con_aporte_con_reforma", height = "300px"),
                   )
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
                   h4("Reforma: Cotización sobre el décimo tercer y cuarto sueldo."),
                   p("Según el Art. 182 del Capítulo Uno (ÁMBITO DE APLICACIÓN, RECURSOS, REQUISITOS Y PRESTACIONES) del Anteproyecto de Ley,
                     la aportación obligatoria que corresponde al afiliado con relación de dependencia, equivalente al 11,06% sobre su remuneración imponible,
                     la misma que se consignará del siguiente modo: la aportación patronal obligatoria equivalente al 4,42%, más la aportación personal
                     obligatoria del trabajador afiliado equivalente al 6,64%. Esta aportación obligatoria del 11,06%, deberá hacerse también sobre el décimo tercer sueldo y el décimo cuarto sueldo. "
                     ),
                   # fluidRow(
                   #   infoBoxOutput("ahorro", width = 3),
                   #   infoBoxOutput("Naportes", width = 3),
                   #   infoBoxOutput("tasa_reemplazo", width = 3),
                   #   infoBoxOutput("cobertura", width = 3)
                   # ),
                   # fluidRow(
                   #   infoBoxOutput('VApension', width = 3),
                   #   infoBoxOutput("pensionpromedio", width = 3),
                   #   infoBoxOutput("pension_teorica_actual", width = 3),
                   #   infoBoxOutput("pension_teorica_jub", width = 3)
                   # ),
                 )
               )
             )
      )
    )
  )
)
