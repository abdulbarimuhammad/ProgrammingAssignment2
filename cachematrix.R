## The functions below generate a matrix and calculate it's inverse if the 
## inverse has not already been computed. If the inverse
## has been computed then it retrieves this inverse stored in cache.

## makeCacheMatrix creates a matrix. It functions to 
## 1. Set the matrix
## 2. Get the values associated with the matrix above
## 3. Set the inverse of the matrix above
## 4. Get the values associated with the inverse matrix above 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following calculates the inverse of the matrix created above in makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. 
## If yes, it skips the computation of the inverse and instead gets it from the cache.
## If not, it calculates the inverse and assigns a new value via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
