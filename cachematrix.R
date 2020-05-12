#This program contains twon functions. Its prupose is to save the computatation time and power
#if the computed value already exists in the cache

## This function makes a Matrix (assuming it to be invertible)
#and it sets and gets the value of matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
    dim_m<- dim(x)
    Inverse_x<- matrix(NA, nrow(x), ncol(x))
    set<- function(y){
        x<<-y
        Inverse_x<<-matrix(NA, nrow(x), nrow(y))
    }
    get<- function() x
    setInverse<- function(Inv)
        Inverse_x<<- Inv
    getInverse<- function()
        Inverse_x
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
    
}


## This function checks that whether the computed value is already in the cache or not
#If not, then it computes the inverse from the scratch but it is already present in the  cache
#then instead of re-calculating the value, it fetches that value.

cacheSolve <- function(x, ...) {
    Inverse_x<- x$getInverse()
    if (!all(is.na(Inverse_x))){
        print("Getting Chached data!")
        return(Inverse_x)
    }
    data<- x$get()
    Inverse_x<- solve(data,...)
    x$setInverse(Inverse_x)
    return(Inverse_x)
}
