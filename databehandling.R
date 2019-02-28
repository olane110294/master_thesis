source("boxplot.R")
library(readxl)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)

dataset <- read_excel("vinmonopolet_auksjoner.xlsx")
colnames(dataset)[19]="vBud"
colnames(dataset)[16]="tilslag_flaske"
colnames(dataset)[15]="tilslag_totalt"
colnames(dataset)[12]="verdi_flaske"
# henter liste alle kjøperne
unikeKjopere_list <- split(dataset,dataset$vBud)
# teller hvor mange auksjoner de har kjøpt flasker på
antRunder <- sapply(unikeKjopere_list, NROW) 
antRunder <- sort(antRunder, decreasing = TRUE)

inspeksjon <- dataset[grep(names(antRunder[i]), dataset$vBud),]
inspeksjon <- inspeksjon[order(inspeksjon$tilslag_flaske), c(5,11,12, 15,16,21, 22, 23,4)]

qplot(antRunder, geom="histogram", binwidth=0.5, main="fordeling av kjøpere",
      xlab = "unike kjøpere", fill=I("blue"), xlim=c(0,50))
# for å sjekke hva de kjøper, så har jeg laget en funksjon som plotter boxplot
# det eneste kriteriet for å kjøre denne funksjonen er å importere dataset og gi nye kolonnenavn
# altså kjøre koden fra linje 1 til og med linje 12
boxInspect(3)

