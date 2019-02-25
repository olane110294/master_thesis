for (i in 1:NTR) {
    X = as.vector(as.matrix(XTR[i,]))
    YZ = FN(W, V, U, X) # return Y and Z
    Y = YZ$Y
    Z = YZ$Z
    for (j in 1:NOUT) {
        F[i+NOUT*(j-1)] = Y[j]-YTR[i,j]
    } # j
} # i

# becomes with a function

for (i in 1:NTR) {
    X = as.vector(as.matrix(XTR[i,]))
    YZ = FN(W, V, U, X) # return Y and Z
    Y = YZ$Y
    Z = YZ$Z
    F[i+NOUT*(1:NOUT-1)] <- sapply(1:NOUT, foo_inner)
} # i

foo_inner <- function(j) {
    return(Y[j]-YTR[i,j])
}


# which simplifies to 

foo_outer <- function(i) {
    X = as.vector(as.matrix(XTR[i,]))
    YZ = FN(W, V, U, X) # return Y and Z
    Y = YZ$Y
    Z = YZ$Z
    F[i+NOUT*(1:NOUT-1)] <- sapply(1:NOUT, foo_inner)
} # i

# The problem now is that foo_inner takes i as parameter, but since it is defined outside the foo_outer
# it will always use the i defined in the environment and not the argument of foo_outer
# two solutions: define foo inner inside foo_outer or let foo_inner take i as parameter

# new foo inner
foo_inner <- function(i) {
    function(j){
        return(Y[j]-YTR[i,j])
        }
}

# so the new foo_outer becomes
foo_outer <- function(i) {
    X = as.vector(as.matrix(XTR[i,]))
    YZ = FN(W, V, U, X) # return Y and Z
    Y = YZ$Y
    Z = YZ$Z
    F[i+NOUT*(1:NOUT-1)] <- sapply(1:NOUT, foo_inner(i))
} # i

# the next modification is that we ensure that we return a value and not do an assignment inside the function

foo_outer <- function(i) {
    X = as.vector(as.matrix(XTR[i,]))
    YZ = FN(W, V, U, X) # return Y and Z
    Y = YZ$Y
    Z = YZ$Z
    return(sapply(1:NOUT, foo_inner(i)))
} # i

# now it is possible to apply the foo_outer function,
lapply(1:NTR,foo_outer)
# since we used lapply, we can collapse it into a one big vector
F <- do.call(c, lapply(1:NTR, foo_outer))

#to get all the j elements from the list
LL <- lapply(1:NTR, foo_outer)
get_j_values <- function(j) {
    sapply(LL, function(x) x[[j]])
}
# for any j value, get_j_values returns a vector of all jth element of LL. Applying it to all j values,
# it returns a list with first element: all "i" values for j=1, then for the second element all "i" for j=2
LL2 <- lapply(1:NOUT, get_j_values)

# then collapse the list into one big vector 
F <- do.call(c,LL2)




