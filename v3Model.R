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

dataset <- read_excel("vinmonopolet_auksjoner.xlsx")
colnames(dataset)[3]="object_id"
colnames(dataset)[19]="kjoper_ID"
colnames(dataset)[16]="tilslag_flaske"
colnames(dataset)[15]="tilslag_totalt"
colnames(dataset)[12]="verdi_flaske"

# lager en dataframe med vinnende bud, reservepris og id på kjoeper
df_testkjoring <- data.frame(bud=round(dataset$tilslag_totalt), 
                             reservePrice=round(dataset$Minstepris), 
                             id_auksjon=dataset$object_id)
# Setter alle observasjoner som har bud mindre eller lik reserverpice 
# til NA, og fjerner disse det gjøres kun fordi vi ikke har lagt inn et 
# ifelse statement i koden. Det gjøres senere
df_testkjoring$bud <- ifelse(df_testkjoring$bud<=df_testkjoring$reservePrice, 
                             NA,df_testkjoring$bud)
df_testkjoring <- df_testkjoring[-which(is.na(df_testkjoring)),]

# her blir dataframe sortert etter størrelsen på rserveprisen. Kun for å unngå 
# ekstreme tilfeller hvor reserveprisen blir veldig høy (for da har det vist 
# seg at algoritmen vår krasjer)
df_testkjoring <- df_testkjoring[order(df_testkjoring$reservePrice),]

# her legger vi inn data til variablene vinnende bud, deltaker per auksjon(random)
# og reserveprisen tilhørende budet som er lagt inn. Siden parameterverdiene blir
# så lav, så skalerer vi ned dataen med 1000. Så alle tall er oppgitt i '000.
bud <- round(df_testkjoring$bud[4000:4500])/1000
deltakere <- round(runif(501,2,10))
r=round(df_testkjoring$reservePrice[4000:4500])/1000

# her lages den likningen av binomiale og pois, som skal integreres over alle
# potensielle bydere, altså store N. Dette er likning (5.3) og (5.4) 
# i masteroppgaven på side 15.
# Det første uttrykket funksjon() spesifiserer hvilket input som må kjøres inn
# mens funksjon(N) er det som bestemmer indikatoren for hele likning_en.
# Siden vi ønsker å integrere likning_en over N, så setter vi inn N. 
# Dette er vist i likning (6.1) i masteroppgaven. 
likning_en <- function(i, scale, shape, lambda){
  function(N){
    
    ngittN = dbinom(deltakere[i], size = N, prob = pWEI2(r[i], scale, shape, 
                                                         lower.tail = FALSE), 
                    log = TRUE)
    
    sN = dpois(N, lambda, log = TRUE)
    
    return(ngittN + sN)
  }}

# her defineres log Likelihood funksjonen for en auksjon i. Legg merke til at
# det er i sapply vi bestemmer hvor stort område vi ønsker å integrere N over.
# Sapply vil iterere fra deltaker[i] til 20 000, og for hver iterasjon vil 
# den bruke funksjonen "likning_en". - Hva blir output i listeObs variabelen?
# Jo, det blir nesten (20 000x1) vektor for auksjon i, som inneholder den
# kalkulerte sannsynligheten for at vi observerer n gitt N.
# legg merke til at her er funksjon(i) den som viser at i denne likningen,
# så kommer vi til å iterere over alle i. 
logLikelihood <- function(scale, shape, lambda){
  function(i){
    listeObs <- sapply(deltakere[i]:20000, likning_en(i, scale, shape, lambda))
    
    sum_listeObs <- sum(listeObs)           
    
    # her regnes ut second-order statistic i likning (5.2) i masteroppgaven, for hver auksjon i.
    secondOrderStatistic = 
      log((deltakere[i])) +
      log((deltakere[i]-1)) + 
      log((pWEI2(bud[i], scale, shape)-pWEI2(r[i], scale, shape)))*
      (deltakere[i]-2) +
      log(pWEI2(bud[i], scale, shape, lower.tail = FALSE)) +
      log(dWEI2(bud[i], scale, shape)) -
      log((1-pWEI2(r[i], scale, shape)))*deltakere[i]
    
    # tallene er i ln(), så derfor kan vi plusse density verdien for n i 
    # auksjon i til (20 000x1) vektoren.  
    logLik <- sum_listeObs + secondOrderStatistic
    
    return(logLik)
  }}

# frem til nå har vi kalkulert log Likelihood gitt parameterne for en auksjon i.
# i secondPart vil vi kalkulere log Likelihood for hver auksjon i. 
# Vi har jo mange i, derfor iterer vi log Likelihood for alle auksjoner i.
# Se likning (6.2) i masteroppgaven. Merk at det første argumentet i pbsapply
# bestemmer hvor mange ganger vi skal iterere i. Note: pbsapply er bare en 
# ekstra feature som gir estimert tid sapply vil bruke per iterasjon i.
sum_LL <- function(scale, shape, lambda){
  # her itererer vi over alle observasjonene, og får ut en vektor med dimensjon
  # (antall observasjoner x1). 
  LogLikelihood_auksjon <- pbsapply(1:length(bud), logLikelihood(scale, shape, 
                                                                 lambda))
  
  # her summer vi alle observasjonene sine log likelihood.
  LL <- -sum(LogLikelihood_auksjon)
  
  print(scale)
  print(shape)
  print(lambda)

  return(LL)
}

# her compiles mle2 for å få det til å gå litt raskere
mle2 <- cmpfun(mle2)

# Endelig skal skal spille sammen. Setter startverdier som bør være i
# ballpark av de optimale parameterne, og må bruke metoden "CG" siden 
# vi bruker alternativ spesifisering av weibull.
result_mle <- mle2(minuslogl = sum_LL, start=list(scale = 0.5,
                                                  shape = 2,
                                                  lambda = 3),
                  method="CG", #lower=c(9.088e-09,1.001,2), nobs = length(bud), 
                  lower = c(scale=9.088e-02, shape=0.0001, lambda=1), 
                  trace = TRUE#, upper = c(scale=0.32, shape=Inf, lambda=8)
                  )


########################################################################
  # Her kan det lages en plot på de estimerte second order statistics og
  # de budene vi observerer i datasettet. For å gjøre det, definerer vi
  # en variabel som estimerer alle SOS for observasjonene, og legger det
  # inn i en dataframe.
SoS <- function(scale, shape){
    # her regnes ut SoS i likning (5.2) i masteroppgaven, for hver auksjon i.
    second_order_statistic = 
      log((deltakere)) +
      log((deltakere-1)) + 
      log((pWEI2(bud, scale, shape)-pWEI2(r, scale, shape)))*
      (deltakere-2) +
      log(pWEI2(bud, scale, shape, lower.tail = FALSE)) +
      log(dWEI2(bud, scale, shape)) -
      log((1-pWEI2(r, scale, shape)))*deltakere
    
    return(second_order_statistic)
}

estimert_SoS = -SoS(1.3, 1.2)

estimertSoS_DF <- data.frame(id=as.numeric(1:length(estimert_SoS)), 
                             estimert_SoS=as.numeric(estimert_SoS))

observert_DF <- data.frame(id=as.numeric(1:length(estimert_SoS)),
                           observert_SoS=log(as.numeric(bud)))

# plotter de estimerte SoS mot budene vi observerer.
ggplot() + 
  # her lages en rød graf for de observerte budene i log format. (base e)
  geom_line(data=observert_DF, aes(x=id, y=bud), color="black") +
  # her lages en blå graf for de estimerte second-order statistics
  # siden de estimerte log sannsynlighetene er i minus, så korrekteres det
  geom_line(data=estimertSoS_DF, aes(x= id, y= estimert_SoS), color="blue") +
  # her setter jeg alt i svart/hvitt, og fjerner bakgrunnslinjene.
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank()) 

########################################################################

 