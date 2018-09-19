## Takes 2x2 matrix as a parameter(Default is used if none given)
## Uses getter and setter methods to retrieve and set new matrix

makeCacheMatrix <- function(x = matrix(c(1,2,3,4), nrow=2, ncol=2)) {
        inv <- matrix(nrow=2,ncol=2)
        set <- function(y) {
                x <<- y
                inv <<- matrix(nrow=2,ncol=2)
        }
        get <- function() x
        setinv <- function(invm) inv <<- invm
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Checkes in cached data if the matrix already exists or finds and sets inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.na(inv[1][1])) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

