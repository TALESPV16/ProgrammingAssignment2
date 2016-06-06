## Put comments here that give an overall description of what your
## functions do
## invMtrx <- NULL   # sets the value invMtrx to NULL (It gives a default for the cacheSolve if it has not been used)
## M       <- NULL   # sets the value M to NULL (It gives a default for the cacheSolve if it has not been used)
## setMtrx <- function(M) # assigns the value of the matrix M to the variable setMtrx
## {
##         x <<- M  # Stash the injected matrix and then the cacheSolve able to check if it was changed 
##         invMtrx <<- NULL # Sets the value of invMtrx(the inverse matrix if used the function cacheSolve) to NULL
## }
## getMtrx <- function() x  # acquire the original matrix
## setInvMtrx <- function() invMtrx <<- solve(x) # calculate the inverse matrix
## getInvMtrx <- function() invMtrx              # acquire the inverse matrix
## list(setMtrx = setMtrx,          # Creating a list that stores the four functions
##      getMtrx = getMtrx,
##      setInvMtrx = setInvMtrx,
##      getInvMtrx = getInvMtrx)

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMtrx <- NULL   
        M       <- NULL   
        setMtrx <- function(M) 
        {
                x <<- M  
                invMtrx <<- NULL 
        }
        getMtrx <- function() x  
        setInvMtrx <- function() invMtrx <<- solve(x) 
        getInvMtrx <- function() invMtrx              
        list(setMtrx = setMtrx,          
             getMtrx = getMtrx,
             setInvMtrx = setInvMtrx,
             getInvMtrx = getInvMtrx)
       
}
funs <- makeCacheMatrix() 
funs$setMtrx(matrix(1:4, 2))
funs$getMtrx()
funs$setInvMtrx()
funs$getInvMtrx()

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$getInverse()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

cacheSolve(x)
