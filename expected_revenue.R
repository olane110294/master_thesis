#### Expected Revenue - "kjent" N ####

# Input - From structural estimation #

N = 10
v0 = 0
r = 0
shape = 5
scale = 1

# Expected Revenue

interior = function(x){
    (x * ((shape/scale) * (x/scale)^(shape - 1) * exp(-(x/scale)^shape)) +
         (1 - exp(-(x / scale) ^ shape)) - 1) * ((1 - exp(-(x / scale) ^ shape)))^(N - 1)
}

Expected_Revenue_fun = function(N, v0, r, shape, scale) {
    ER = v0 * (1 - exp(-(r / scale) ^ shape)) ^ N +
        N * integrate(interior, r, Inf)$value
    return(ER)
}               

Expected_Revenue_fun(N, v0, r, shape, scale)

# Plot of Expected Revenue
N_seq = seq(1,1000,by = 1)
ExR = Expected_Revenue_fun(N_seq, v0, r, shape, scale)
plot(N_seq,
     ExR,
     type = "l")
