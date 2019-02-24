setwd("C:\\Users\\ola_k\\OneDrive\\Skrivebord\\Master Thesis\\Data")

library(tidyr)
library(dplyr)
library(stats4)
library(gamlss.dist)
library(ggplot2)
library(ggthemes)
library(lmtest)


fileWin <- read.delim("win.dat", header=FALSE, stringsAsFactors = FALSE)
fileWin$V1 <- gsub("[[:space:]]+", ",", fileWin$V1)
fileWin$V1 <- gsub("^,", "", fileWin$V1)
fileWin <- fileWin %>% 
    separate(V1, sep = ",", c("id", "vinBud", "heterogenitet", "N"))
x <- list(fileWin)
id <- as.numeric(fileWin$id)
bud <- as.numeric(fileWin$vinBud)
deltakere <- as.numeric(fileWin$N)
kovariat <- as.numeric(fileWin$heterogenitet)
r=0.5

# f?rste fors?k p? ? lage likelihood funksjonen v?r
eq1 <- function(theta1, theta2, lambda){
     # definerer to vektorer slik at R skal g? raskere. vektorN bestemmer
    # hvor mange N vi skal summe over.
        vektorN = (1:100)
        loopVerdi = 0
            if(deltakere[i]>=2){        
                for (N in deltakere[i]:20000) {
                    # lager alle delene i likelihood separat
                    density = (
                        (N) *
                        (N-1) * 
                        (pWEI2(bud[i], theta1, theta2))^(N-2) *
                        # lower.tail sp?r om det er snakk om CDF (TRUE) eller 1-CDF (FALSE)
                        pWEI2(bud[i], theta1, theta2, lower.tail = FALSE) *
                        dWEI2(bud[i], theta1, theta2)) /
                        (1-(pWEI2(r, theta1, theta2))^N)
                    
                    ngittN =
                        dbinom(deltakere[i], size = N, 
                               prob = pWEI2(r, theta1, theta2, lower.tail = FALSE))
                    sN = 
                        dpois(N, lambda)
                   # her er sannsynligheten for observasjon i
                    loopVerdi = loopVerdi + (density * ngittN * sN)
                }
            return(loopVerdi)
            }
    }

hello <- function(theta1, theta2, lambda) {
        ola <- 1:length(bud)
            for (i in 1:length(bud)){
                ola[i] <- eq1(theta1=1, theta2=2, lambda=7)
                print(i)
            }
        return(ola)
        }

test <- hello(theta1=1,theta2=2,lambda=7)


# denne funksjonen fungerer ikke som den skal. Det blir produsert det samme tallet for alle observasjoner
ola <- 1:length(bud)
for (i in 1:length(bud)){
    ola[i] <- eq1(1, 2, 7)
}


