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
library(compiler)

df_testkjoring <- data.frame(bud=round(dataset$tilslag_totalt), reservePrice=round(dataset$Minstepris), id=dataset$object_id)
df_testkjoring$bud <- ifelse(df_testkjoring$bud<=df_testkjoring$reservePrice, NA,df_testkjoring$bud)
df_testkjoring <- df_testkjoring[-which(is.na(df_testkjoring)),]

bud <- df_testkjoring$bud[1:100]
deltakere <- round(rpois(100,3))
deltakere <- ifelse(deltakere>=2, deltakere,2) # fordi det er ikke lagt inn et if statement her enda
r=df_testkjoring$reservePrice[1:100]

bud <- 

foo_inner <- function(i, theta1, theta2, lambda){
    function(N){
    
    ngittN = log(dbinom(deltakere[i], size = N, prob = pWEI2(r, theta1, theta2, lower.tail = FALSE)))
    
    sN = log(dpois(N, lambda))

    return(ngittN + sN)
    }}

foo_outer <- function(theta1, theta2, lambda){
            function(i){
            listeObs <- sapply(deltakere[i]:20000, foo_inner(i, theta1, theta2, lambda))
            
            density = 
            log((deltakere[i])) +
            log((deltakere[i]-1)) + 
            log((pWEI2(bud[i], theta1, theta2)-pWEI2(r[i], theta1, theta2)))*(deltakere[i]-2) +
            log(pWEI2(bud[i], theta1, theta2, lower.tail = FALSE)) +
            log(dWEI2(bud[i], theta1, theta2)) -
            log((1-pWEI2(r, theta1, theta2)))*deltakere[i]
            
            listeObs = listeObs + density
            
            return(sum(listeObs))
            }}

eqThree <- function(theta1, theta2, lambda){
    secondPart <- pbsapply(1:length(bud), foo_outer(theta1, theta2, lambda))
    
    LL <- -sum(log(secondPart))
    print(theta1)
    print(theta2)
    print(lambda)
    return(LL)
}

foo_inner <- cmpfun(foo_inner)
foo_outer <- cmpfun(foo_outer)
eqThree <- cmpfun(eqThree)
mle <- cmpfun(mle)
result_mle <- mle(minuslogl = eqThree, start=list(theta1 = 0.1,
                                                  theta2 = 0.2,
                                                  lambda = 5),
                  method="L-BFGS-B", lower=c(0.1,0.1,3), upper=c(5,0.999, 10),
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

