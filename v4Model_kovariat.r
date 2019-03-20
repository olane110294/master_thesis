library(bbmle)
library(compiler)
library(pbapply)
library(xlsx)

subDataset <- read.xlsx("data_python_inklkovariat.xlsx", sheetName = "Sheet1")
# Det er en observasjon som gir -Inf, og det er # 1625
subDataset <- subDataset[-1625,]

bud <- subDataset$winbid
deltakere <- subDataset$nobs
r <- subDataset$rprice
dBurg <- subDataset$dummyBurgund
dPiem <- subDataset$dummyPiemonte
mean(bud)
mean(deltakere)

likning_en <-
    function(i,
             shape1,
             shapeBurg,
             shapePiem,
             scale1,
             scaleBurg,
             scalePiem,
             lambda) {
        function(N) {
            ngittN = dbinom(
                deltakere[i],
                size = N,
                prob = pweibull(
                    r[i],
                    exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                    exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i]),
                    lower.tail = FALSE
                ),
                log = FALSE
            )
            
            sN = dpois(N, lambda, log = FALSE)
            
            return(ngittN * sN)
        }
    }

logLikelihood <-
    function(shape1, shapeBurg, shapePiem, scale1, scaleBurg, scalePiem, lambda) {
        function(i) {
            listeObs <- sapply(0:50, likning_en(i, shape1, shapeBurg, shapePiem,
                                                scale1, scaleBurg, scalePiem, 
                                                lambda))
            
            log_sum_listeObs <- log(sum(listeObs))
            
            # her regnes ut second-order statistic i likning (5.2) i masteroppgaven, for hver auksjon i.
            if (deltakere[i] >= 2) {
                secondOrderStatistic =
                    log((deltakere[i])) +
                    log((deltakere[i] - 1)) +
                    log((pweibull(
                        bud[i],
                        exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                        exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i])
                    ) -
                        pweibull(
                            r[i],
                            exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                            exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i])
                        ))) *
                    (deltakere[i] - 2) +
                    log(pweibull(
                        bud[i],
                        exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                        exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i]),
                        lower.tail = FALSE
                    )) +
                    log(dweibull(
                        bud[i],
                        exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                        exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i])
                    )) -
                    log(1 - pweibull(
                        r[i],
                        exp(shape1 + shapeBurg * dBurg[i] + shapePiem * dPiem[i]),
                        exp(scale1 + scaleBurg * dBurg[i] + scalePiem * dPiem[i])
                    )) * deltakere[i]
            } else {
                secondOrderStatistic = log(1)
            }
            
            logLik <- log_sum_listeObs + secondOrderStatistic
            
            return(logLik)
        }
    }
sum_LL <-
    function(shape1,
             shapeBurg,
             shapePiem,
             scale1,
             scaleBurg,
             scalePiem,
             lambda) {
        LogLikelihood_auksjon <-
            pbsapply(
                1:length(bud),
                logLikelihood(
                    shape1,
                    shapeBurg,
                    shapePiem,
                    scale1,
                    scaleBurg,
                    scalePiem,
                    lambda
                )
            )
        
        LL <- -sum(LogLikelihood_auksjon)
        
        print(shape1)
        print(shapeBurg)
        print(shapePiem)
        print(scale1)
        print(scaleBurg)
        print(scalePiem)
        print(lambda)
        
        return(LL)
    }

mle2 <- cmpfun(mle2)

result_mle <- mle2(
    minuslogl = sum_LL,
    start = list(
        shape1 = 2,
        shapeBurg = 0.1,
        shapePiem = 0.2,
        scale1 = 8,
        scaleBurg = 0.2,
        scalePiem = 0.2,
        lambda = 5
    )
)


shape1 <- result_mle@coef[["shape1"]]
shapeBurg <- result_mle@coef[["shapeBurg"]]
shapePiem <- result_mle@coef[["shapePiem"]]
scale1 <- result_mle@coef[["scale1"]]
scaleBurg <- result_mle@coef[["scaleBurg"]]
scalePiem <- result_mle@coef[["scalePiem"]]
lambda <- result_mle@coef[["lambda"]]

save.image("estimert_kovariat.RData")

# Kjørte et testrun 19.03. kl 22:00 uten kovariat. 
# Python: lambda = 8.06225, shape = 1.0695876 og scale = 1512.04966 
# R:    lambda = 7.986, shape = 1.079 og scale = 1529.3

summary(subDataset[subDataset$dummyBurgund == 1, ]$winbid) # BURGUND
summary(subDataset[subDataset$dummyPiemonte == 1, ]$winbid) # PIEMONTE
summary(subDataset[subDataset$dummyBurgund == 0 &
                       subDataset$dummyPiemonte == 0, ]$winbid) # BORDEAUX

weibull_mean <-
    function(shape1,
             shapeBurg,
             shapePiem,
             scale1,
             scaleBurg,
             scalePiem) {
        mean <-
            exp(scale1 + scaleBurg * dBurg + scalePiem * dPiem) *
            gamma(1 + 1 / exp(shape1 + shapeBurg * dBurg + shapePiem * dPiem))
        return(mean)
    }
weibull_mean(2, 0.1, 0.2, 8, 0.2, 0.2)
