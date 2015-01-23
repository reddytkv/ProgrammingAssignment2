## This program contains 2 function, makeCacheMatrix & CacheSolve.
## which help to reach data from matrix and return inverted matrix.
## if the matrix is already present, it returns from Cache, else it is created and returned.

## This function created a necessary functions to set or get matrix.
## Finally this function returns a matrix.
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(mat) m <<- mat
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

###---------------------------------------------------------


## This metric returns a inverse of the a input metrix; if..
## if the metrics is alreay present get it from Cache, 
## else calculate and get it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m<-x$getmatrix()
		if(!is.null(m)){
			message("getting inverted matrix")
			return(solve(m))
		}
		
		mat <- x$get()
		m <- solve(mat,...)
		mat$setmatrix(m)
		m
}

###---------------------------------------------------------
