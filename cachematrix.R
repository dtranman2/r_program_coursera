# Take an argument 'x' which has to be a square matrix in order 
# to work properly. If the matrix is a square matrix, then 
# it will compute the inverse of the matrix. Next it 
# will save the result to a global variable. If the same matrix is 
# called again to calculate the same matrix inverse, it will just
# use the saved result in the global variable. 

# function will allow you to set and get the matrix variable,
# along with being able to set and get the inverse of the matrix
makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # 
    get <- function() {
        x
    }
    
    setinverse <- function(inverse) {
        m <<- inverse
    }
    
    getinverse <- function() {
        m
    }
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# Function will compute the inverse of the matrix, but will 
# check to see if it has been saved before
# If it has been computed prior, it will just take the global
# variable saved instead of doing the same calculation again.
cacheSolve <- function(x, ...) {
    
    # Go and get the saved variable from getinverse method 
    m <- x$getinverse()
    
    # Check 'm' value if it is null or not.  If null, it will 
    # compute the inverse.  If not null, it will just use the cache data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)

    m
}
