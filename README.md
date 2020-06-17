# SIR
#This a SIR model for Colombia, using Johns Jopkins University's data. When runned the model is update at the last data date #provided. The code is in R and was done for Data Mining course at Universidad de los Andes with Maria Andrea Dominguez, Juan #Andres Rincon and Martin Lara.

install.packages('R0')
install.packages('devtools')
install.packages('tsiR')

library(readr) # Webscraping
library(dplyr) # Manejo de bases de datos
library(tidyverse)
library(devtools)
library(R0)    # Libreria epidemiologica
library(tsiR)  # Libreria modelo SIR en series de tiempo



# 1. Descargar los datos
confirmed <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
deaths <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
recovered <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')

#Transponer las bases
Fecha <- seq(as.Date("2020-01-22"), as.Date("2020-05-31"), by="days") %>% as.data.frame() %>% rename("Fecha"=".")

#Confirmados
confirmed <- confirmed[83,2:ncol(confirmed)]
confirmed <- t(confirmed)
confirmed <- confirmed[4:nrow(confirmed),]
confirmed <- cbind(Fecha,confirmed)
confirmed <- confirmed %>% rename("Confirmados"="confirmed") %>% mutate(Confirmados=as.numeric(as.character(Confirmados)))

#Muertes
deaths <- deaths[83,2:ncol(deaths)]
deaths <- t(deaths)
deaths <- deaths[4:nrow(deaths),]
deaths <- cbind(Fecha,deaths)
deaths <- deaths %>% rename("Muertes"="deaths") %>% mutate(Muertes=as.numeric(as.character(Muertes)))

#Recuperados
recovered <- recovered[83,2:ncol(recovered)]
recovered <- t(recovered)
recovered <- recovered[4:nrow(recovered),]
recovered <- cbind(Fecha,recovered)
recovered <- recovered %>% rename("Recuperados"="recovered") %>% mutate(Recuperados=as.numeric(as.character(Recuperados)))

#Merge
Colombia <- confirmed %>% merge(.,deaths, by = "Fecha") %>% merge(.,recovered, by = "Fecha")
Colombia <- Colombia %>% mutate(Población=50882884) %>% 
          mutate(TasaRecup=Recuperados/Confirmados) %>% 
          mutate(Infectados=Confirmados-Muertes) %>%  
          mutate(Susceptibles=Población-Infectados) %>% 
          mutate(RecupYFallecidos=Recuperados+Muertes)

#Parámetros
  

# 3. Calcular el R0
Colombia$ro <- seq(1:nrow(Colombia))
time = generation.time('lognormal', c(3.96, 4.75))
casos <- Colombia[nrow(Colombia), 2]


r0 <- est.R0.EG(Colombia$Confirmados, time, t = Colombia$Fecha, begin = Colombia$Fecha[1], end = Colombia$Fecha[nrow(Colombia)])
r0 <- r0$R 

# 4. Construir SIR
t_r <- Colombia$TasaRecup[nrow(Colombia)]
S <- Colombia$Susceptibles[nrow(Colombia)]/Colombia$Población[nrow(Colombia)]
I <- Colombia$Infectados[nrow(Colombia)]/Colombia$Población[nrow(Colombia)]
R <- Colombia$RecupYFallecidos[nrow(Colombia)]/Colombia$Población[nrow(Colombia)]

parametros <- c(beta = r0, gamma = t_r)
initials <- c(S = S, I = I, R = R)

SIR <- SIR(pars = parametros, init = initials, time = 0:nrow(Colombia))
PlotMods(SIR)
