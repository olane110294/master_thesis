


## Weibull mean function ##
weibull.mean = function(theta1, theta2) {
    {
        mean = theta2 ^ (-1 / theta1) * gamma(1 + 1 / theta1)
        return(mean)
    }
}

## Parameter Testing ##

weibull.mean(5, 1)

scale = 1
shape = 5

mean_2 = scale ^ (-1 / shape) * gamma((1 / shape) + 1)
mean_2

seq = seq(0, 4, by = 0.0001)

plot(
    seq,
    dWEI2(seq,
          mu = scale,
          sigma = shape),
    col = "red",
    type = "l",
    ylab = "",
    xlab = ""
)
