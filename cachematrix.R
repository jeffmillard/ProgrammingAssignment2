## =============================================================

## Write a short comment describing this function
## =============================================================

makeCacheMatrix <- function(x = matrix()) {
#        makeVector <- function(x = numeric()) {
#                m <- NULL
#                set <- function(y) {
#                        x <<- y
#                        m <<- NULL
#                }
#                get <- function() x
#                setmean <- function(mean) m <<- mean
#                getmean <- function() m
#                list(set = set, get = get,
#                     setmean = setmean,
#                     getmean = getmean)
#        }
        
}


## =============================================================
## Function:    cacheSolve <- function(x, ...) 
## 
## Description: Return a matrix that is the inverse of 'x'
##
## Arguments:   x
##
## Returns:
##
## Notes:       

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## replace with inverse ??
        m <- x$getmean() # not mean, change this
        
        ## probably again check if it is not null
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if it is null, calculate inverse
        data <- x$get()
        m <- mean(data, ...) # not mean, change
        x$setmean(m)
        m
}       
        cachemean <- function(x, ...) {
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
}
