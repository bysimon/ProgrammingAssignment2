##-----------------------------------------------------------------------------
#
# Program: cachematrix.R
#
# Objective: This program contains 2 functions when performed in combination (refer to suggestion below) should cache
#            required time-consuming computations. It takes advantage of the scoping rules of the R language and how
#            they can be manipulated to preserve state inside of an R object.
#
##-----------------------------------------------------------------------------



##-----------------------------------------------------------------------------
#
# Function: makeCacheMatrix
#
# Objective: This function creates a special "matrix" object that can cache its inverse.
#            
#
# Input: x of type matrix
#
# Output: 
#
#            
# Assumptions: the matrix supplied is always invertible.
#
##-----------------------------------------------------------------------------
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setmean <- function(mean) m <<- mean
    
    getmean <- function() m
    
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

##-----------------------------------------------------------------------------
#
# Function: cacheSolve
#
# Objective: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
#            already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from 
#            the cache.
#
#
# Input: x of type matrix
#
# Output: return a matrix that is the inverse of 'x'
#
# Assumptions:
#
##-----------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getmean()
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    m <- mean(data, ...)
    
    x$setmean(m)
    
    m
}

## -----------------------------------------------------------------

