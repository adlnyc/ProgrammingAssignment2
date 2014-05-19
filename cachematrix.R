
#################################################################################
##
##  Matrix inversion is usually a costly computation and there may be some 
##  benefit to caching the inverse of a matrix rather than computing it 
##  repeatedly.  The two functions below take advantage of the scoping rules of 
##  the R language to cache an input square matrix, x, and its inverse once the 
##  inverse has been computed and to then retrieve the inverse from cache anytime 
##  it is subsequently needed.
##
##  If x changes, the new value of x will then be cached and the value of its 
##  inverse stored in cache will be set to NULL indicating that it needs to be
##  recomputed the next time the inverse is needed.
##
#################################################################################


#################################################################################
##
##  The first function, `makeCacheMatrix` takes a square matrix, x, as input
##  (this matrix is assumed to be invertible) and creates as output a special 
##  "matrix", which is really a list containing functions to:
##
##  1.  set the value of the input matrix, i.e.,
##      a.  store x in cache and 
##      b.  store NULL as the value of its inverse in cache;
##  2.  get the value of the input matrix from cache;
##  3.  set the value of x inverse, i.e., place x inverse in cache;
##  4.  get the value of x inverse from cache.
##
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(xinv) inv <<- xinv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#################################################################################
##
##  The second function, `cacheSolve` is subsequently called to compute the 
##  inverse of the input matrix.  The input to this function is the output list
##  list from `makeCachematrix`.  The output is the inverse of x.
##
##  It first checks to see if the value of the inverse stored in cache is NULL. 
##  If it is NOT, then the inverse has already been computed and it is simply 
##  retrieved from cache.  If the value  in cache is NULL, then the "solve" 
##  function is called to compute the inverse and this value is then stored in cache.
##
#################################################################################

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'.  As this might be very large,
    ## it is returned invisibly.

    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(invisible(inv))
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    invisible(inv)
}
