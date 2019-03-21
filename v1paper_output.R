### OUTPUT DESCRIPTIVES ###

#### FUNCTIONS ####

# Weibull mean function #
weibull.mean = function(shape, scale) {
    {
        mean = scale * gamma(1 + 1/shape)
        return(mean)
    }
}

#### DESCIPTIVES ####

## Plots ####

## Mean ####

weibull.mean(0.9507171,1000.022)

## Optimal Reserve Price ####

## Expected Revenue ####

## Consumer Surplus ####

