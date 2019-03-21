#### TESTING ####
## Testing - Mean ####

weibull.mean(0.9507171,1000.022)  # Input
weibull.mean(0.9507171,1000.022)  # Output

## Testing - side by side plot ####

par(mfrow=c(1,2))

plot(
    dweibull(0:1000, 0.9507171, 1000.022),  # Input
    type = "l",
    main = "",
    xlab = "",
    ylab = ""
)

lines(dweibull(0:1000, 0.89, 932), type = "l")  # Output

plot(
    dpois(0:20, 8.05177),  # Input
    type = "l",
    main = "",
    xlab = "",
    ylab = ""
)

lines(dpois(0:20, 4), type = "l")  # Output


## Testing - top to bottom plot ####

par(mfrow=c(2,1))

plot(
    dweibull(0:2000, 0.85, 833.58),  # Input
    type = "l",
    main = "",
    xlab = "",
    ylab = ""
)

lines(dweibull(0:2000, 0.8846, 889.30), type = "l")  # Output

plot(
    dpois(0:20, 5),  # Input
    type = "l",
    main = "",
    xlab = "",
    ylab = ""
)

lines(dpois(0:20, 8.05177), type = "l")  # Output

