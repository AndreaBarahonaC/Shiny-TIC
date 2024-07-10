library(shiny)
library(lubridate)
library(lifecontingencies)
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)
library(shinythemes)
library(shinydashboard)
library(DT)
library(highcharter)

# COEFICIENTES

Coeficiente <- data.frame(An.Imposiciones = c(5:40),
                          Coef = c(0.4375, 0.4500, 0.4625,0.4750,0.4875, 0.5000, 0.5125, 0.5250, 0.5375, 0.5500, 0.5625, 0.5750, 0.5875, 0.6000, 0.6125, 0.6250, 
                                   0.6375, 0.6500, 0.6625, 0.6750, 0.6875, 0.7000, 0.7125, 0.7250, 0.7375, 0.7500, 0.7625, 0.7750, 0.7875, 0.8000, 0.8125, 0.8325,
                                   0.8605, 0.8970, 0.9430, 1.0000))
for (i in 41:100) { # Añadir nuevas filas para cuando supera los 40 años de aportes
  ultimo_coef <- tail(Coeficiente$Coef, 1)
  nuevo_coef <- ultimo_coef + 0.0125
  nueva_fila <- data.frame(An.Imposiciones = i, Coef = nuevo_coef)
  Coeficiente <- rbind(Coeficiente, nueva_fila)
}




# Funciones

# Función para ajustar la pensión según mínimos
ajustar_pension_min <- function(numero_imposiciones, pension, rango_valores) {
  for (rango_valor in rango_valores) {
    rango <- rango_valor$rango
    valor_punto <- rango_valor$valor_punto
    if (!is.na(valor_punto) && !is.na(pension) & rango[1] <= numero_imposiciones & numero_imposiciones <= rango[2]) {
      if (pension < valor_punto) {
        pension <- valor_punto
      }
    }
  }
  return(pension)
}


# Función para ajustar la pensión según máximos
ajustar_pension_max <- function(numero_imposiciones, pension, rango_valores) {
  for (rango_valor in rango_valores) {
    rango <- rango_valor$rango
    valor_punto <- rango_valor$valor_punto
    if (!is.na(valor_punto) && !is.na(pension) & rango[1] <= numero_imposiciones & numero_imposiciones <= rango[2]) {
      if (pension > valor_punto) {
        pension <- valor_punto
      }
    }
  }
  return(pension)
}






# Progresión Geometrica 
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

# Condiciones Mínimas 
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

#  Se usa por defecto la siguiente información obtenida del IESS
# Aporte del afiliado (personal y patronal) al seguro= 11,06% de su salario
# Tasa de crecimiento de salarios= 2.154%



