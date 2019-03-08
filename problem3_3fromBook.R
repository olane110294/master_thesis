library(tidyr)
library(dplyr)
# library(neldermead) I think this package is redundant
library(stats4)
library(ggplot2)
library(gamlss.dist)

#######################
#    OPPGAVE 3.3.C    #
#######################

# Load and catalog the data. Must clean up the data in order for it to be ready
fileWin <- read.delim("win.dat", header=FALSE, stringsAsFactors = FALSE)
# using regex to seperate columns
fileWin$V1 <- gsub("[[:space:]]+", ",", fileWin$V1)
fileWin$V1 <- gsub("^,", "", fileWin$V1)
# create a new data frame with four columns
win <- fileWin %>% separate(V1, sep = ",", c("id", "vinBud", "heterogenitet", "N"))
# lager variablene
W <- as.numeric(win[,2])
N <- as.numeric(win[,4])
r <- 0.5
T <- length(W)
Ns <- unique(N)

# her lager jeg modellen som er oppgitt i boken. Definisjonen av second-order
# statistic
first_equation_c <- function(theta1, theta2){
  part1 = 
    log(N) + 
    log(N-1) + 
    (N-2)*log(pWEI2(W,theta1,theta2)) + 
    log(1 - pWEI2(W,theta1,theta2)) + 
    log(dWEI2(W,theta1,theta2)) - 
    log(1 - (pWEI2(r,theta1,theta2))^N)
  return(part1)
}

# for aa sjekke hvor godt parameterne treffer, saa kan vi inspisere 
# dataen for aa sjekke matchen. Dette er ble fremhevet som et viktig element 
# Du kan proeve aa sette inn forskjellige verdier for thetaene. her lager jeg
# second order statistics gitt parameterne 0.5 og 1.
estimert_SoS <- first_equation_c(theta1=0.5,theta2=1)
# her lager jeg dataframe med de estimerte second order statistics
prelim <- data.frame(id=(1:50), secondOrderStatistics=estimert_SoS) 

ggplot() + 
geom_line(data=win, aes(x=as.numeric(id),y=log(as.numeric(vinBud))), color="red") + # her må jeg logge budene 
geom_line(data=prelim, aes(x=as.numeric(id),y=-as.numeric(secondOrderStatistics)), color="blue") + # tallene utregnet er i minus, så må ta minus foran pre
theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# her kaller jeg Log Likelihod funksjonen vår, og summerer den for alle observasjonene i
fun33c <- function(theta1,theta2){
  part1 <- first_equation_c(theta1, theta2)
  # her summer jeg alle observasjonene
    L <- -sum(part1)
    print(theta1)
    print(theta2)
    return(L)
}

# bruker MLE til å optimisere
result_mle <- mle(minuslogl = fun33c,  start = list(theta1=0.5,theta2=0.5),
                  method = "L-BFGS-B", lower=c(0.1, 0.1), upper=c(Inf,Inf),
                  control=list(trace=TRUE), nobs = length(W)
                  )

result_mle
# Save the output
theta_1 <- result_mle@coef[["theta1"]]
theta_2 <- result_mle@coef[["theta2"]]

# Save the function value at the parameter estimates (this will be used later)
Fval_c <- fun33c(theta_1,theta_2)

# Save the Hessian (this will be used later in the question).
Hessian <- result_mle@details[["hessian"]]




#######################
#    OPPGAVE 3.3.H    #
#######################

  
# definerer disse variablene globalt i R. Jeg tror det er best ? definere disse globalt.
# legg merke til at vi definerer kovariaten Z.
W <- as.numeric(win[,2])
Z <- as.numeric(win[,3])
N <- as.numeric(win[,4])
T <- length(win$id)
r <- 0.5

# henter datapunktet utenifra, derfor trenger jeg ikke ? spesifisere den inn 
# i funksjonen. Legg merke til at for å legge til kovariat, så erstatter
# jeg bare "theta1" ved likningen theta1=theta10+theta11*Z. Se boken for
# å se det hele utrykket
first_equation_h = function(theta10,theta11,theta2){
  first = 
    log(N) + 
    log(N-1) + 
    (N-2)*log(pWEI2(W,exp(theta10+theta11*Z),theta2)) +
    log(1-pWEI2(W,exp(theta10+theta11*Z),theta2)) + 
    log(dWEI2(W,exp(theta10+theta11*Z),theta2)) -
    log(1-(pWEI2(r,exp(theta10+theta11*Z),theta2))^N)
  
    return(first)
  }
###############################################################################################
# for aa sjekke hvor godt parameterne treffer, saa kan vi inspisere 
# dataen for aa sjekke matchen. Dette er ble fremhevet som et viktig element 
# Du kan proeve aa sette inn forskjellige verdier for thetaene 
# legger inn minus foran for å gjøre det sammenlignbart med de observerte budene
estimert_SoS <- -first_equation_h(theta10=-0.2, theta11 = 0, 
                              theta2= 1.4)
# her lager jeg en dataframe med de estimerte Second-Order statistics
prelim <- data.frame(id=as.factor(win$id), estimertSOS=estimert_SoS)

ggplot() + 
geom_line(data=win, aes(x=as.numeric(id),y=log(as.numeric(vinBud))), color="red") + # her må jeg logge budene 
geom_line(data=prelim, aes(x=as.numeric(id),y=as.numeric(estimert_SoS)), color="blue") + # tallene utregnet er i minus, så må ta minus foran pre
theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
###############################################################################################

# lager second_equation. Her henter jeg funksjonen som vi definerte ovenfor
# og summerer for alle observasjonene.
fun33h = function(theta10,theta11,theta2){
  second = first_equation_h(theta10, theta11, theta2) 
  
  LL = -sum(second)
  print(theta10)
  print(theta11)
  print(theta2)
  
  return(LL)
}

# her bruker jeg standard MLE for å finne parameterne
result_mle <- mle(minuslogl = fun33h, start = list(theta10 = -.2,theta11 = 0, 
                                                   theta2 = 1.5),
                  lower=list(-2,-1,0), upper=list(theta10=Inf, theta11=0.2, 
                                                  theta2=Inf),
                  method= "L-BFGS-B"
                  )


result_mle
# Save the output
theta_10 <- result_mle@coef[["theta10"]]
theta_11 <- result_mle@coef[["theta11"]]
theta_2 <- result_mle@coef[["theta2"]]

# Save the function value at the parameter estimates (this will be used later)
Fval_h <- fun33h(theta_10, theta_11, theta_2)

# Save the Hessian (this will be used later in the question).
Hessian <- result_mle@details[["hessian"]]

# likelihood ratio test done manually. At p=5% and 1 degree of freedom, the 
# chi-square critical value is 3.8414

# lager en funksjon som tester likelihood sannsynligheten
likelihood_test <- function(old, new){
  LR <- 2*(old-new)
    
  if (LR>3.8414){
     print("Reject") 
  } else {
      print("Fail to reject")
    }
  }

likelihood_test(Fval_c, Fval_h)
