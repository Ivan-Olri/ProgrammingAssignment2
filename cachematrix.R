## This function creates a list that stores the four elements needed to set 
## and cache the inverse

makeCacheMatrix = function(x=matrix()){
        i = NULL
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinverse = function(inverse) i <<- inverse
        getinverse = function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function will retrieve the cached data for the inverse if it exists already,
##otherwise it will set the inverse and cache it.

cacheSolve = function(x, ...) {
        i = x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data = x$get()
        i = solve(data, ...)
        x$setinverse(i)
        i
}