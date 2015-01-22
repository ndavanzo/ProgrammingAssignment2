## These functions are able to compute and cache the inverse of a matrix.
## If the cache is empty the inverse is performed. Otherwise not.
## This is useful to increase the computation speed.

## This first function creates a special "matrix" object that can cache its inverse (by using the second function).
## It is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(matrix) m<<-matrix
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m ## Return a matrix that is the inverse of 'x'
}
