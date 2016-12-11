## Matrix Inversion : Benefitting from the Caching rather than computing every time
## For this Two funcitons are created ;One wil define the matrix/Functions the other will inverse 



## functions 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invers <- NULL
        set <- function(y) {
                x <<- y
                invers <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invers <<- inverse
        getInverse <- function() invers
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getInverse()
        if (!is.null(invers)) {
                message("getting cached data")
                return(invers)
        }
        mat <- x$get()
        invers <- solve(mat, ...)
        x$setInverse(invers)
        invers
}



