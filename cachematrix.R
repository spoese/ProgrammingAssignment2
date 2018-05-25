## These two functions interact to efficiently return the inverse of a 
## square invertible matrix. makeCacheMatrix creates the functions required
## in order for cacheSolve to calculate or simply return the inverse of the
## original matrix.

## If stored into an object say by calling myMatrix <- makeCacheMatrix(x) where
## x is a square invertible matrix, then the function myMatrix$set(y) will
## set the matrix passed into cacheSolve to be y.
## myMatrix$get is used in cacheSolve to get the matrix defined using either
## makeCacheMatrix or myMatrix$set.
## myMatrix$setinverse is used in cacheSolve to assign the inverse matrix.
## Lastly, myMatrix$getinverse allows cacheSolve to check whether the inverse
## has already been solved for.

## The function makeCacheMatrix creates a list of functions (set,get,setinverse,
## getinverse) that form the basis necessary for cacheSolve (the second
## function) to act on.

makeCacheMatrix <- function(x = matrix()) {
        ## initializes the value for i
        i <- NULL
        ## creating the $set function that will initialize our matrix x
        ## and the inverse i in the parent environment
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## create the $get function that pulls the value of x
        get <- function() x
        ## create the $setinverse function that gives a value to i in the
        ## parent environment
        setinverse <- function(inverse) i <<- inverse
        ## create the $getinverse function that pulls the value of i
        getinverse <- function() i
        ## returns a list of the four functions created
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## cacheSolve uses the list created in makeCacheMatrix to efficiently return
## the inverse of the original input. If it has already been computed (and
## cached) it returns that value without recalculating it.

cacheSolve <- function(x, ...) {
        ## pulls the value of i from the parent environment (possibly NULL)
        i <- x$getinverse()
        ## If the inverse is not null, it means it has already been calculated.
        ## Thus, the function will print a message that it has been cached
        ## and returns the cached value without any further calculation.
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## stores the matrix into the variable 'data'
        data <- x$get()
        ## calculates the inverse of the 'data' matrix (originally x)
        i <- solve(data, ...)
        ## stores the inverse into i in the parent environment
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
        i
}
