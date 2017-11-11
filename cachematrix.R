##  This is a pair of functions that cache the inverse of a matrix.

## This function takes matrix as input and carry out hte inverse and stores in
## Cache

makeCacheMatrix <- function(x = matrix())
{
    invrs <- NULL
    setit <- function(y) 
    {
        x <<- y
        invrs <<- NULL
    }
    getit <- function() x
    
    setInver <- function(inverse) invrs <<- inverse
    
    getInver <- function() invrs
    
    list(setit = setit,
         getit = getit,
         setInver = setInver,
         getInver = getInver)

}


## This funcion first check if the inverse of the supplied matrix is already 
## calculated and stored in the cache if yes then pull the result from cache 
## else calculate the same 

cacheSolve <- function(x, ...)
    {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInvers()
    if (!is.null(invrs))
    {
        message("Retriving cached data")
        return(invrs)
    }
    mat <- x$getit()
    invrs <- solve(mat, ...)
    x$setInvers(invrs)
    invrs
}
