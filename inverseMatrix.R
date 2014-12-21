#   Value of x is set to matrix and m is set to NULL when makeCacheMatrix is invoked
#   Example invocation :  mat <- matrix(data = rexp(200, rate = 10), nrow =2,ncol=2)
#     , a <- makeCacheMatrix(mat)  ,  cacheSolve(a)
#   get() gets the value of matrix
#   setmat() sets the inverse of matrix and called from cacheSolve()
#   getmat() returns the inverse matrix ( cache ) or NULL  


makeCacheMatrix <- function(x) {
m <- NULL
set  <- function(y) {
   x <<- y
   m <<- NULL
}

get <- function() x
setmat <- function(mat) m <<- mat
getmat <- function() m
list( set = set, get = get, setmat = setmat, getmat= getmat)
}


# This routine checks if getmat() is cached. If cached return the inverse else
# find inverse using solve()  

cacheSolve <- function(x, ...) {
        m <- x$getmat()
        if(!is.null(m))
        {
            message("getting cached data")
            return(m)
        }
        data <- x$get() 
        m <- solve(data,...)
        x$setmat(m)
        m      
}

# Example output
# > source("matrix.R")
# >  a <- makeCacheMatrix(mat)
# >  cacheSolve(a)
#          [,1]      [,2]
#[1,]  8.763485 -17.17272
#[2,] -1.550154  40.28837
#> cacheSolve(a)
#getting cached data
#          [,1]      [,2]
#[1,]  8.763485 -17.17272
#[2,] -1.550154  40.28837
#> 

