#//////////////////////////////////////////////
#This code was developed by:
#Crystal English
#R Programming - Assignment 2:1
#Create a cache function
#/////////////////////////////////////////////

## =============================================
## some seriously helpful resources:
## http://www.r-bloggers.com/environments-in-r
## http://www.tandfonline.com/doi/pdf/10.1080/10618600/1996/10474713
## The Art of R Programming
## The R Book
## =============================================


## This function creates a special "matrix" object that can cache
## its inverse, which is then called by cacheSolved()


makeCacheMatrix <- function(x = matrix()) {
        
        # Create a matrix and clear cache by
        # setting it to NULL. Also create getters
        # and setters (a term from Java - fun times!)
        invMatrix <- NULL
        set <- function(y) {
                x <<- y
                invMatrix <- NULL
        }
        
        get <- function() x
        setInv <- function(xInv) invMatrix <<- xInv
        getInv <- function() invMatrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invMatrix <- x$getInv()
        if(!is.null(invMatrix)) {
                message("getting cached inverse matrix")
                return(invMatrix)
        }
        
        message("now calculating inverse matrix")
        dataMatrix <- x$get()
        invMatrix <- solve(dataMatrix, ...)
        x$setInv(invMatrix)
        
        invMatrix
}
