# Creación de índices de la base de datos
library(highcharter)
library(lubridate)
library(lifecontingencies)
library(openxlsx)
library(readxl)
library(tidyverse)
library(data.table)

load("obtencion_pension_prom.Rdata")
names(pensiones2)
unique(pensiones2$tipo_beneficiario)

# Indicadores sobre población pensionada ---------------------------------------
pensiones2 <- pensiones2 %>% mutate(anios_derecho_concedido=floor(as.numeric(ymd(fecha_inicial_pension)-ymd(fecha_derecho))/365))

pen_fem <- pensiones2 %>%
  mutate(edad_hoy=floor(as.numeric(ymd(today())-ymd(fecha_nacimiento_concesion))/365),
         anios_derecho_concedido=floor(as.numeric(ymd(fecha_inicial_pension)-ymd(fecha_derecho))/365) )  %>% #select(edad_hoy) %>% 
  filter(edad_hoy<=108,
         numero_imposiciones_totales>=10,
         valor_pension_concedida>0,
         edad_jubilacion>=numero_imposiciones_totales/12 + 18) %>% 
  select(edad_hoy,
         edad_jubilacion,
         promedio_sueldo_real,
         numero_imposiciones_totales,
         valor_acumulado,
         coeficiente_real,
         valor_pension_concedida,
         fecha_derecho,
         anios_derecho_concedido,
         fecha_inicial_pension
  ) %>%  summary()

view(pen_fem)

a <- which(pensiones2$valor_pension_concedida==10.9000)
summary(pensiones2$fecha_nacimiento_concesion[419])
summary(as.factor(pensiones2$sexo))
summary(ymd(pensiones2$fecha_inicial_pension),probs = seq(0,1,0.01))

###############################################################################

# Analisis para los afiliados:

list.files()
load("bases_originales/bases_cotizacion.Rdata")
load("bases_originales/afiliados.Rdata")
load("bases_originales/KSRECTPLANILLAS.RData")

names(bases_cotizacion)
summary(as.factor(bases_cotizacion$sexo))
# sexo
edad
valaponor
summary(bases_cotizacion$valaponor) #existen valores negativos?? xd


names(afiliados)
summary(as.factor(afiliados$estfal))
afiliados %>% filter(estfal==0,numimp>0) %>% select(numimp) %>% summarise(
  Q0 = quantile(numimp, probs = 0),
  Q25 = quantile(numimp, probs = 0.25),
  Q50 = quantile(numimp, probs = 0.5),
  Q75 = quantile(numimp, probs = 0.75),
  Q100 = quantile(numimp, probs = 1),
  mean = mean(numimp)
)
which(afiliados$feciniafi=="0101-03-20")
summary(afiliados$numimp)

sexo
edad
estfal #si esta vivo o fallecido
feciniafi #Fecha de inicio de la relación de afiliacion
numimptot #Número de imposiciones totales del afiliado
estafi #Estado del afiliado
numimp #Número de Imposiciones
fecini #Fecha de Ingreso con el empleador
unique(afiliados$numjub) #quesignifica que alguien tenga 6 jubulaciones?



