## R Programming (Coursera Course, John Hopkins University)
## Programming Assigment 2: caching potentially time-consuming computations
## Teresa Correas Ubiera (October 2015)

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse, which 
## is really a list containing a function to:
##      (1) set the value of the matrix
##      (2) get the value of the matrix
##      (3) set the value of the inverse
##      (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setmatriz<- function(y){
                x<<-y
                m<-NULL
                }
        getmatriz<- function() x
        setmatrizinversa<- function(inverse)
                m<<-inverse
        getmatrizinversa<- function() m
        list(setmatriz=setmatriz, getmatriz=getmatriz, setmatrizinversa=setmatrizinversa, 
             getmatrizinversa=getmatrizinversa)
        }

## Function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        m<-x$getmatrizinversa()
        if (!is.null(m)){
                message("getting cached matrix")
                return(m)
                }
        inmatrix<- x$getmatriz()
        m<-solve(inmatrix, ...)
        x$setmatrizinversa(m)
        m
        }
