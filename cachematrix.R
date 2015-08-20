## Assignment: Caching the Inverse of a Matrix

## The first function, makeVector creates a special "vector", which is really a list 
# containing a function to 
# 1.set the value of the vector
# 2.get the value of the vector
# 3.set the value of the mean
# 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y){
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse
        getinverse = function() inv
        list(inv=inv, set=set, setinverse=setinverse, getinverse=getinverse)
}


## The following function calculates the mean of the special "vector" created with 
# the above function. However, it first checks to see if the mean has already been 
# calculated. If so, it gets the mean from the cache and skips the computation. 
# Otherwise, it calculates the mean of the data and sets the value of the mean in the
# cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        # checks to see if the inverse has already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise, it calculates the inverse of the data and sets the value of the 
        # mean in the cache via the setmean function.
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinverse(inv)
        inv
}
