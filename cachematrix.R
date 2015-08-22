## Two functions that create an environment where the inverse of a matrix can be calculated and cached.
## If the inverse is stored, the second function will retrieve it from the list created in the first.
## Otherwise it will calculate the inverse and cache it.

## makeCacheMatrix creates a list where the inverse of a matrix can be cached.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve looks to see if the inverse of a matrix is cached as i.  If it is, the inverse is returned.
## if it isn't, i is calculated and set (cached).

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message('getting cached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
        ## Return a matrix that is the inverse of 'x'

