packages <- c(
    "tidyr",
    "dplyr",
    "stats4",
    "gamlss.dist",
    "ggplot2",
    "ggthemes",
    "lmtest",
    "reshape",
    "data.table",
    "pbapply",
    "compiler",
    "bbmle"
)

install.packages(packages)
library(readxl)
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

dataset <- read_excel("vinmonopolet_auksjoner.xlsx")
colnames(dataset)[3] = "object_id"
colnames(dataset)[19] = "vBud"
colnames(dataset)[16] = "tilslag_flaske"
colnames(dataset)[15] = "tilslag_totalt"
colnames(dataset)[12] = "verdi_flaske"

# henter bud for å inspisere bud under reserveprice osv
df_testkjoring <- data.frame(
    bud = round(dataset$tilslag_totalt),
    reservePrice = round(dataset$Minstepris),
    id = dataset$object_id
)
df_testkjoring$bud <-
    ifelse(df_testkjoring$bud <= df_testkjoring$reservePrice,
           NA,
           df_testkjoring$bud)
df_testkjoring <- df_testkjoring[-which(is.na(df_testkjoring)),]

df_testkjoring <-
    df_testkjoring[order(df_testkjoring$reservePrice),]

# for å generere tall fra weibull,
# bare kjør rWEI2(antall, parameter1, parameter2)

bud <- round(df_testkjoring$bud[4000:4100]) / 1000
deltakere <- round(rpois(101, 3))
deltakere <- ifelse(deltakere >= 2, deltakere, 2)
r = round(df_testkjoring$reservePrice[4000:4100]) / 1000

foo_inner <- function(i, scale, shape, lambda) {
    function(N) {
        ngittN = dbinom(
            deltakere[i],
            size = N,
            prob = pWEI2(r[i],
                         scale,
                         shape,
                         lower.tail = FALSE),
            log = TRUE
        )
        
        sN = dpois(N,
                   lambda,
                   log = TRUE)
        
        return(ngittN + sN)
    }
}

#foo_inner <- memoise(foo_inner)

foo_outer <- function(scale, shape, lambda) {
    function(i) {
        listeObs <- sapply(deltakere[i]:20000,
                           foo_inner(i,
                                     scale,
                                     shape,
                                     lambda))
        
        density =
            log((deltakere[i])) +
            log((deltakere[i] - 1)) +
            log((pWEI2(bud[i],
                       scale,
                       shape) - pWEI2(r[i],
                                       scale,
                                       shape))) * (deltakere[i] - 2) +
            log(pWEI2(bud[i],
                      scale,
                      shape,
                      lower.tail = FALSE)) +
            log(dWEI2(bud[i],
                      scale,
                      shape)) -
            log((1 - pWEI2(r[i],
                           scale, shape))) * deltakere[i]
        
        listeObs = listeObs + density
        
        return(sum(listeObs))
    }
}

#foo_outer <- memoise(foo_outer)
eqThree <- function(scale, shape, lambda) {
    secondPart <- pbsapply(1:length(bud),
                           foo_outer(scale,
                                     shape,
                                     lambda))
    
    LL <- -sum(secondPart)
    print(scale)
    print(shape)
    print(lambda)
    
    return(LL)
}

mle2 <- cmpfun(mle2)
result_mle <- mle2(
    minuslogl = eqThree,
    start = list(
        scale = 5,
        shape = 1,
        lambda = 3
    ),
    method = "CG",
    #lower=c(9.088e-09,1.001,2), nobs = length(bud),
    #lower = c(
        #scale = 0,
        #shape = 0,
        #lambda = 1
    #),
    trace = TRUE#, upper = c(scale=0.32, shape=Inf, lambda=8)
)
