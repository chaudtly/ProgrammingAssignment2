## The functions together create a matrix, check to see if the  
## inverse has been solved, if so it gets the inverse from the cache,
## if not, solves for the inverse

## Creates special matrix containing a function to 
## set the the matrix, get the matrix, set the solved matrix
## and get the solved matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Solves inverse of matrix, but first checks to see if matrix is in 
## cache; If so, gets cached inverse, if not solves

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)                
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}
