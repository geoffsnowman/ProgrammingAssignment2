## These functions create a cache that allows the user to invert
## a matrix once and then return the results of the solve function 
## as needed.

## This function creates the special cache list that stores the 
## inverted matrix. The object returned by the matrix is a list of 
## four functions: set, get, setInverse and getInverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function calls getInv to try to get the inverse.
## If the result of getInv is not null, then it returns 
## that result.
## If the result of getInv is null, it solves the matrix,
## then caches the inverted matrix using setInv.
## Either way, it returns the inverse matrix to the caller. 

cacheSolve <- function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    message("solving the matrix")
    m <- solve(data, ...)
    x$setInv(m)
    m
}

## The following code will test the functions

r1 <- c(3,0,0)
r2 <- c(0,3,0)
r3 <- c(0,0,3)
m <- rbind(r1,r2,r3)
c <- makeCacheMatrix(m)
## First call should solve the matrix
cacheSolve(c)
## Second call should use the cache
cacheSolve(c)
## Third call should use the cache
cacheSolve(c)