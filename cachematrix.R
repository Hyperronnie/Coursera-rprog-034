## Programming Assignment 2: Caching the Inverse of a Matrix

## In this assignment, I will creat two functions to 
## cache the calculation results of "matrix inversion"

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
    # initial value
    m_inv <- NULL
    # signals for whether the calculation for inversion is needed
    # 0 - no; 1 - yes
    signal <- 1
    # function to set the matrix
    set <- function(new_m) {
        m <<- new_m
        m_inv <<- NULL
        signal <<- 1    # the matrix has been changed
    }
    # function to get the matrix
    get <- function() m
    # function to set the inversion
    setinv <- function(new_inv) m_inv <<- new_inv
    # function to get the inversion
    getinv <- function() m_inv
    # function to set the signal
    setsignal <- function(new_signal) signal <<- new_signal
    # function to get the signal
    getsignal <- function() signal
    # return value as a list
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv, 
         setsignal = setsignal,
         getsignal = getsignal)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(m, ...) {
    if(m$getsignal() == 0) {
        m_inv = m$getinv()
        message("getting cached data")
        return(m_inv)
    }
    data <- m$get()
    m_inv <- solve(data, ...)
    m$setinv(m_inv)
    m$setsignal(0)
    m_inv
}
