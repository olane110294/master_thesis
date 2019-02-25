setwd("C:\\Users\\ola_k\\OneDrive\\Skrivebord\\Master Thesis\\Git_Rcode\\master_thesis")

library(tidyr)
library(dplyr)
library(stats4)
library(gamlss.dist)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(reshape)
require(data.table)


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

# her er del en av likelihood funksjonen 
eqOne <- function(i,theta1, theta2, lambda){
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

# here is the second equation in our likelihood estimate.
# these results are not log transformed. 
eqTwo <- function(theta1, theta2, lambda) {
        firstPart <- 1:length(bud)
            for (i in 1:length(bud)){
                firstPart[i] <- eqOne(i,theta1, theta2, lambda)
               # print(i)
            }
        return(firstPart)
        }

# third equation which will sum all the observations and write 
# out the final likelihood function
eqThree <- function(theta1, theta2, lambda){
    secondPart <- eqTwo(theta1, theta2, lambda)
    
    LL <- -sum(log(secondPart))
    #print(theta1)
    #print(theta2)
    print(lambda)
    return(LL)
}

result_mle <- mle(minuslogl = eqThree, start=list(theta1 = 1,
                                                  theta2 = 2,
                                                  lambda = 7),
                  method="L-BFGS-B", lower=c(0,0,0),
                  nobs = length(bud), control=list(maxit = 10))

#save.image("v1Model.RData")

# code for plotting the estimated values after eqTwo
ggplot() + 
    geom_line(data=win, aes(x=as.numeric(id), y=log(as.numeric(vinBud)), 
                             color="species_id")) +
    geom_line(data=prelim, aes(x=as.numeric(id),y=-as.numeric(pre), 
                               color="Species")) +
    theme_economist() + theme(legend.position="top") +  
    scale_color_discrete(name = "", labels = c("Estimated", "Observed"))
df <- data.frame(bud=bud, deltakere=deltakere)
ggplot(df, aes(x = bud)) + geom_density()

# create a vector for densities for the observed and the estimated bids
dEstimated <- dWEI2(bud, 1.171147, 1.783813) 
dObserved <- approx(density(bud)[["y"]])[["y"]]

# make the data ready for plotting. Go from wide to long dataset
oDF <- data.frame(bud=as.factor(bud), estimated = dEstimated, observed=dObserved)
oDF <- melt(oDF)

# scale the variables so it is possible to plot them together
oDT <- data.table(oDF)    
oDT <- oDT[,scaled:= scale(value), by= "bud"]
str(oDT)    

ggplot(oDT) +
geom_density(aes(x = scaled, color = variable)) + theme_economist() +
scale_color_discrete(name = "", labels = c("Estimated", "Observed"))









