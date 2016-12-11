## As in the given example for makeVector, I plan to create a special matrix
## that contains a function to: set and get the value
## of the matrix, and then set and get the inverse of the matrix 

## My aim is to create a special function - matrix that stores the value
## of the input in a variable m.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(solve) m <<- solve
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## I will create a function that computes the inverse of the matrix.
## If the inverse is already calculated the function will get it from the cache
## If the inverse has not been calculated, the matrix from makeCacheMatrix 
## will be used, m will calculate the inverse and x$setinverse(m) will store
## it in makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
