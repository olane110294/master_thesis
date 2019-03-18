library(readxl)
library(tidyr)
library(tidyverse)
library(plyr)

budlogg <- read_excel("datasett_auksjoner.xlsx")
budlogg$Tidspunkt <- as.POSIXlt(budlogg$Tidspunkt)

# Siden filen som er laget i excel er en flat liste, så må det gjøres noen 
# manipuleringer for å få det i liste form inn i R.

# Denne koden vil gi alle loggene et objektnummer i kolonnen ved siden av

# henter plasseringen til objektnummerne og lager en liste som skal brukes til
# å finne ut hvor mange ganger objektnummer skal iterere
plassering_objektnummer <- grep("[[:digit:]]+", budlogg$Objektnummer)
iterasjoner = 0 
for (i in 1:length(plassering_objektnummer)) {
    iterasjoner[i] <- plassering_objektnummer[i+1] - plassering_objektnummer[i]
}
# må spesifisere siste runde siden kalkulasjonen ikke tar høyde for det.
iterasjoner[length(iterasjoner)] <- 8
# henter ut alle objektnummerene
objektnummer <- grep("[[:digit:]]+", budlogg$Objektnummer, value=TRUE)
# allokerer objektnummer til alle entries i loggen
budlogg$Objektnummer <- rep(objektnummer, times = iterasjoner)
# fjerner alle NA
budlogg <- na.omit(budlogg)
# make a list of the 'budlogg'
budlogg_list <- dlply(budlogg, .(Objektnummer), c)
count <- unique(budlogg$Objektnummer) # check if we get the same, as we do

# her er budloggen vår i dataframe
budlogg
# her er budloggen vår i listeform
budlogg_list
