#### LOGNORMAL ####

library(tidyr)
library(dplyr)
library(stats4)
library(gamlss.dist)
library(ggplot2)
library(ggthemes)
library(lmtest)
library(rescale)
library(data.table)
library(pbapply)
library(compiler)
library(bbmle)
library(readxl)
library(stats)
dataset <- read_excel("vinmonopolet_auksjoner.xlsx")
colnames(dataset)[3] = "object_id"
colnames(dataset)[19] = "kjoper_ID"
colnames(dataset)[16] = "tilslag_flaske"
colnames(dataset)[15] = "tilslag_totalt"
colnames(dataset)[12] = "verdi_flaske"

#### Testing av Mortens data ####

bud = as.numeric(simulert_data$winbid)
r = as.numeric(simulert_data$rprice)
deltakere = as.numeric(simulert_data$nobs)

#####

# her lages den likningen av binomiale og pois, som skal integreres over alle
# potensielle bydere, altså store N. Dette er likning (5.3) og (5.4)
# i masteroppgaven på side 15.
# Det første uttrykket funksjon() spesifiserer hvilket input som må kjøres inn
# mens funksjon(N) er det som bestemmer indikatoren for hele likning_en.
# Siden vi ønsker å integrere likning_en over N, så setter vi inn N.
# Dette er vist i likning (6.1) i masteroppgaven.
likning_en <- function(i, mean, sd, lambda) {
    function(N) {
        ngittN = dbinom(
            deltakere[i],
            size = N,
            prob = plnorm(r[i], mean, sd,
                            lower.tail = FALSE),
            log = FALSE
        )
        
        sN = dpois(N, lambda, log = FALSE)
        
        return(ngittN * sN)
    }
}

# her defineres log Likelihood funksjonen for en auksjon i. Legg merke til at
# det er i sapply vi bestemmer hvor stort område vi ønsker å integrere N over.
# Sapply vil iterere fra deltaker[i] til 20 000, og for hver iterasjon vil
# den bruke funksjonen "likning_en". - Hva blir output i listeObs variabelen?
# Jo, det blir nesten (20 000x1) vektor for auksjon i, som inneholder den
# kalkulerte sannsynligheten for at vi observerer n gitt N.
# legg merke til at her er funksjon(i) den som viser at i denne likningen,
# så kommer vi til å iterere over alle i.
logLikelihood <- function(mean, sd, lambda) {
    function(i) {
        listeObs <-
            sapply(0:50, likning_en(i, mean, sd, lambda))
        
        sum_listeObs <- sum(listeObs)
        
        # her regnes ut second-order statistic i likning (5.2) i masteroppgaven, for hver auksjon i.
        if (deltakere[i] >= 2) {
            secondOrderStatistic =
                ((deltakere[i])) +
                ((deltakere[i] - 1)) +
                ((plnorm(bud[i], mean, sd) - plnorm(r[i], mean, sd))) *
                (deltakere[i] - 2) +
                (plnorm(bud[i], mean, sd, lower.tail = FALSE)) +
                (dlnorm(bud[i], mean, sd)) -
                ((1 - plnorm(r[i], mean, sd))) * deltakere[i]
        } else {
            secondOrderStatistic = 1
        }
        
        # tallene er i ln(), så derfor kan vi plusse density verdien for n i
        # auksjon i til (20 000x1) vektoren.
        logLik <- sum_listeObs * secondOrderStatistic
        
        return(logLik)
    }
}
# frem til nå har vi kalkulert log Likelihood gitt parameterne for en auksjon i.
# i secondPart vil vi kalkulere log Likelihood for hver auksjon i.
# Vi har jo mange i, derfor iterer vi log Likelihood for alle auksjoner i.
# Se likning (6.2) i masteroppgaven. Merk at det første argumentet i pbsapply
# bestemmer hvor mange ganger vi skal iterere i. Note: pbsapply er bare en
# ekstra feature som gir estimert tid sapply vil bruke per iterasjon i.
sum_LL <- function(mean, sd, lambda) {
    # her itererer vi over alle observasjonene, og får ut en vektor med dimensjon
    # (antall observasjoner x1).
    LogLikelihood_auksjon <-
        pbsapply(1:length(bud), logLikelihood(mean, sd,
                                              lambda))
    
    # her summer vi alle observasjonene sine log likelihood.
    LL <- -sum(LogLikelihood_auksjon)
    
    print(mean)
    print(sd)
    print(lambda)
    
    return(LL)
}

# her compiles mle2 for å få det til å gå litt raskere
mle2 <- cmpfun(mle2)

# Endelig skal skal spille sammen. Setter startverdier som bør være i
# ballpark av de optimale parameterne, og må bruke metoden "CG" siden
# vi bruker alternativ spesifisering av lnorm.
result_mle <- mle2(
    minuslogl = sum_LL,
    start = list(
        mean = 2500,
        sd = 5000,
        lambda = 5
    ),
    method = "L-BFGS-B",
    lower = c(0.1, 0.1, 0.1),
    #, upper=c(5,5,10) #, nobs = length(bud),
    #lower = c(mean=1.2e-06, sd=1.2e-06, lambda=1.2e-06),
    # method="CG",
    trace = TRUE #, upper = c(mean=5, sd=Inf, lambda=15)
)


########################################################################
# Her kan det lages en plot på de estimerte second order statistics og
# de budene vi observerer i datasettet. For å gjøre det, definerer vi
# en variabel som estimerer alle SOS for observasjonene, og legger det
# inn i en dataframe.
SoS <- function(mean, sd) {
    # her regnes ut SoS i likning (5.2) i masteroppgaven, for hver auksjon i.
    second_order_statistic =
        log((deltakere)) +
        log((deltakere - 1)) +
        log((plnorm(bud, mean, sd) - plnorm(r, mean, sd))) *
        (deltakere - 2) +
        log(plnorm(bud, mean, sd, lower.tail = FALSE)) +
        log(dlnorm(bud, mean, sd)) -
        log((1 - plnorm(r, mean, sd))) * deltakere
    
    return(second_order_statistic)
}

estimert_SoS = -SoS(1.3, 1.2)

estimertSoS_DF <- data.frame(id = as.numeric(1:length(estimert_SoS)),
                             estimert_SoS = as.numeric(estimert_SoS))

observert_DF <- data.frame(id = as.numeric(1:length(estimert_SoS)),
                           observert_SoS = log(as.numeric(bud)))

# plotter de estimerte SoS mot budene vi observerer.
ggplot() +
    # her lages en rød graf for de observerte budene i log format. (base e)
    geom_line(data = observert_DF, aes(x = id, y = bud), color = "black") +
    # her lages en blå graf for de estimerte second-order statistics
    # siden de estimerte log sannsynlighetene er i minus, så korrekteres det
    geom_line(data = estimertSoS_DF, aes(x = id, y = estimert_SoS), color =
                  "blue") +
    # her setter jeg alt i svart/hvitt, og fjerner bakgrunnslinjene.
    theme_bw() + theme(panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

########################################################################

