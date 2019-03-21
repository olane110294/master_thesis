#### Expected Revenue - "kjent" N ####

# Input - From structural estimation #

N = 31
v0 = 0
r = 1000
shape = 3
scale = 1500

# Expected Revenue

interior = function(x){
    (x * ((shape/scale) * (x/scale)^(shape - 1) * exp(-(x/scale)^shape)) -
         exp(-(x / scale) ^ shape)) * ((1 - exp(-(x / scale) ^ shape)))^(N - 1)
}

Expected_Revenue_fun = function(N, v0, r, shape, scale) {
    ER = v0 * (1 - exp(-(r / scale) ^ shape)) ^ N +
        N * integrate(interior, r, 100000)$value
    return(ER)
}               

Expected_Revenue_fun(N, v0, r, shape, scale)

# Plot of Expected Revenue IKKE SIKKER PÅ OM DETTE STEMMER
N_seq = seq(1,N,by = 1)

ER_vec = sapply(N_seq, Expected_Revenue_fun, v0, r, shape, scale)
ER_vec

plot(N_seq,
     ER_vec,
     type = "l")

# Test #

test = function(x){
    2*x
}

integrate(test, 0, 1 )
