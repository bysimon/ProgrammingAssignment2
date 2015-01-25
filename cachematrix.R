##----------------------------------------------------------------------------------------------------------------------
# Program: cachematrix.R
#
# Objective: This program contains 2 functions when performed in combination (refer to suggestion below) should cache
#            required time-consuming computations. It takes advantage of the scoping rules of the R language and how
#            they can be manipulated to preserve state inside of an R object.
#
#            Refer below for more detailed explanation for each function.
##----------------------------------------------------------------------------------------


##----------------------------------------------------------------------------------------
# Function: makeCacheMatrix
#
# Objective: This function creates a special "matrix" object that can cache its inverse.
#            
#
# Input: x of type matrix
#
# Output: list of four methods to deal with the matrix such as: 
#         - set       : sets the matrix value
#         - get       : gets the matrix value
#         - setInverse: caches the inverted matrix
#         - getInverse: returns the cached inverted matrix, or NULL if it does not exist
#
#            
# Assumptions: the matrix supplied is always invertible.
#
# Usage:
# myMatrix = matrix( c( 1 , 0 , 5  ,  2 , 1 , 6  ,  3 , 4 , 0 ), nrow = 3 , ncol = 3)
# myCache <- makeCacheMatrix( myMatrix )   #  cache for the 1st time
# cInverse <- cacheSolve( myCache )        # calculates the inverse matrix for the 1st time
# cInverse <- cacheSolve( myCache )        # does NOT calculate now, as it has been cached already
#                                          # should get the message "getting cached data"
# # changing the content of the matrix
# myCache$set( matrix( c( 2,1,0,0  ,  3,0,2,2  ,   1,3,-3,3  ,5,1,2,1 ), nrow = 4 , ncol = 4) )   
# cInverse <- cacheSolve( myCache )        # it has to calculate again due to the change into the matrix content, so the cache is useless
# cInverse <- cacheSolve( myCache )        # now it's using from the cache
#                                          # should get the message "getting cached data"
# myCache$get()                            # shows what is in the cache             
# cInverse                                 # shows the inverse of the matrix
#
# 
##---------------------------------------------------------------------------------------
makeCacheMatrix <- function( x = matrix() ) {
    
    m <- NULL            ## reset local m variable
    
    set <- function(y) {
        x <<- y           ## back up current matrix value into x
        m <<- NULL        ## reset m in parent environment
    }
    
    get <- function() x   ## getter method of the matrix
    
    setInverse <- function( mInverse ) m <<- mInverse  ## caching the inverse of the matrix in the parent environment
    
    getInverse <- function() m                         ## getting the cached inversed matrix
    
    ## it returns the list containing the methods to deal with the matrix
    list( set = set
        , get = get
        , setInverse = setInverse 
        , getInverse = getInverse )
}

##----------------------------------------------------------------------------------------
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
# Assumptions: the matrix supplied is always invertible.
#
# Usage: see makeCacheMatrix function comments above
##---------------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x' if exists, otherwise will be null
    m <- x$getInverse()
    
    ## checks if m exists, it was cached before it returns its value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## in case there is no cached value
    data <- x$get()
    
    ## creates the inverted matrix
    m <- solve(data, ...)
    
    ## caches the inverted matrix
    x$setInverse(m)
    
    ## return the inverted matrix
    m
}

##-------------------------- end of file ----------------------------------------------------------------------------------------

