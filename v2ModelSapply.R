packages <- c("tidyr", "dplyr", "stats4", "gamlss.dist", "ggplot2",
              "ggthemes", "lmtest", "reshape", "reshape", "data.table",
              "pbapply")
install.packages(packages)
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

bud <- runif(10, 1.1,1.55)
deltakere <- round(runif(10,5,9))

foo_inner <- function(i, theta1, theta2, lambda){
    function(N){
    density = (
        (N) *
        (N-1) * 
        (pWEI2(bud[i], theta1, theta2))^(N-2) *
        # lower.tail spør om det er snakk om CDF (TRUE) eller 1-CDF (FALSE)
        pWEI2(bud[i], theta1, theta2, lower.tail = FALSE) *
        dWEI2(bud[i], theta1, theta2)) /
        (1-(pWEI2(r, theta1, theta2))^N)
    
    ngittN =
        dbinom(deltakere[i], size = N, 
               prob = pWEI2(r, theta1, theta2, lower.tail = FALSE))
    sN = 
        dpois(N, lambda)

    return(density * ngittN * sN)
    }}

foo_outer <- function(theta1, theta2, lambda){
            function(i){
            listeObs <- sapply(deltakere[i]:20000, foo_inner(i, theta1, theta2, lambda))
            return(sum(listeObs))
            }}

# her er sannsynligheten for hver observasjon, gitt ukjent N
eqThree <- function(theta1, theta2, lambda){
    secondPart <- pbsapply(1:length(bud), foo_outer(theta1, theta2, lambda))
    
    LL <- -sum(log(secondPart))
    return(LL)
}

result_mle <- mle(minuslogl = eqThree, start=list(theta1 = 1,
                                                  theta2 = 2,
                                                  lambda = 7),
                  method="L-BFGS-B", lower=c(0.1,0.1,5),
                  nobs = length(bud))

theta_1 <- result_mle@fullcoef[["theta1"]]
theta_2 <- result_mle@fullcoef[["theta2"]]
elambda <- result_mle@fullcoef[["lambda"]]


#######################################################################
############## plotting the estimated density #########################
######################################################################
x <- as.numeric(seq(0,3.5,0.01))
df <- data.frame(x=x, density=dWEI2(x, 1.171147, 1.78381))
budDensity <- density(bud)
df2 <- data.frame(x=budDensity[["x"]], y=budDensity[["y"]])
ggplot() + 
    geom_line(data=df, aes(x=x, y=density,color="species_id"), size=1) +
    geom_line(data=df2, aes(x=x,y=y,color="Species"), size=1) +
    theme_economist() + theme(legend.position="top") +  
    scale_color_economist(name = "", labels = c("Observed", "Estimated")) + 
    theme(panel.grid.major = element_blank())

