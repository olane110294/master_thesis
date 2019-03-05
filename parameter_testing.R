
## Weibull mean function ##
weibull.mean = function(theta1, theta2){{
    mean = theta2^(-1/theta1)*gamma(1 + 1/theta1)
    return(mean)}
}

## Parameter testing ##
weibull.mean(0.47,0.05)
weibull.mean(0.48,0.05)
weibull.mean(0.49,0.05)
weibull.mean(0.5,0.05)

