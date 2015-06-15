## these functions solve() a given matrix and cache the results for later retrieval

## makeCacheMatrix stores 4 functions set, get, setinverse, and getinverse.
## set changes the matrix stored in x
## get returns the matrix stored in x for return later
## setinverse stores the inverse matrix of x
## getinverse stores the inverse matrix of x for return later

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## i is set to the value of getinverse above. if no value was assigned is NULL.
## if getinverse is not NULL, returns matrix stored in getinverse
## else use matrix stored in get and run function solve()
## store results of solve() in setinverse

cacheSolve <- function(x = matrix(), ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
