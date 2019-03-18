library(tidyr)
library(dplyr)
library(stats4)
library(gamlss.dist)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(reshape)
library(data.table)
library(pbapply)
library(compiler)
library(bbmle)
library(readxl)

# Simulerer data
r <- runif(1000, 0, 1)
storeN <- round(rpois(1000, 5))
SoS = 0
lilleN=0

# her vil både second-order statistic og lille n bli generert.
for (i in 1:length(storeN)) {
    if (storeN[i]>=2){
        budene <- rWEI2(storeN[i], 0.9, 2)
        # finner second order statistic
        SoS[i] <- sort(budene, decreasing = TRUE)[2]
        # finner lille n
        budOverR <- budene[which(budene >= r[i])]
        lilleN[i] <- length(budOverR)
    } else {
        SoS[i] <- NA
        lilleN[i] <- NA
    }
}
# lager dataframe og fjerner alle observasjoner som enten har 1) n<=1 eller 2) bud<=r
simulert_df <- data.frame(bud=as.numeric(SoS), 
                          deltakere=as.numeric(lilleN), 
                          r=as.numeric(r))

# setter alle med bud under minstepris til NA
simulert_df$bud <- ifelse(simulert_df$bud<=simulert_df$r, NA, simulert_df$bud)
simulert_df$bud <- ifelse(simulert_df$deltakere==0, NA, simulert_df$bud)
simulert_df$bud <- ifelse(simulert_df$deltakere==1, simulert_df$r, simulert_df$bud)

# fjerner alle NAs
simulert_df <- na.omit(simulert_df)


bud <- simulert_df$bud
deltakere <- round(simulert_df$deltakere)
r= simulert_df$r

# bruk disse datapunktene rett inn i v3Model.R filen og kjør MLE2. 
bud <- bud[1:900]
deltakere <- deltakere[1:900]
r <- r[1:900]
