## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# set the value of the matrix
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
# get the value of the matrix
    get <- function() x
# set the value of the inverse
    setinverse <- function(solve) m <<- solve
# get the value of the inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
# check to see whether the inverse has been calculated
# if it has been, skip the calculation and return the inverse from the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
# if it hasn't been, get it
    rawdata <- x$get()
# define the size of the matrix and turn the vector into a matrix
    nn<-length(rawdata)
    size<-sqrt(nn)
    data<-matrix(rawdata, nrow=size, ncol=size)
# calculate the inverse and ...
    m <- solve(data, ...)
# ... set the inverse in the cache
    x$setinverse(m)
    m
}
