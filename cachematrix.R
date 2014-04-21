## The goal of these two functions is to get the inverse of an invertible  matrix, 
## sparing computational time if such an inverse has already been computed. 
## If the inverse has already been computed then the inverse is returned without computing it again
## otherwise the inverse is computed with the function "solve" and cached for future reference

## makeCacheMatrix defines a new "object" with 4 methods (returned within a list):
## set() defines the matrix
## get() read the matrix
## setinverse() defines the inverse of the matrix
## getinverse() read the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #when the function makeCacheMatrix is called the inverse is re-set to NULL 
  inverse<-NULL  
  # when the method "set" is used, x is re-set according to input and inverse is re-set to NULL.
  # this must be done in the parent environment, hence <<- is used instead of <- that has a local meaning
  set<- function(y) {  
   x<<-y
   inverse<<-NULL
 }
 get<-function() x
 setinverse<-function(y) inverse<<-y
 getinverse <-function() inverse
 list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## CacheSolve takes as input an object x defined with makeCacheMatrix
## CacheSolve using the method "getinverse" of makeCacheMatrix reads the value of the inverse of x
## if the value is null then it actually computes the inverse of x, returns it and caches it in x using the method "setinverse" of x
## if the value is not null then it returns it, pointing out that it used cached data

cacheSolve <- function(x, ...) {
  # cacheSolve reads the inverse associated to x (with the method getinverse of x)
  inverse<-x$getinverse()
  # if the inverse has not been computed or it has been reset to NULL (e.g., the matrix has changed)
  # then the function skips the following three lines
  # if the inverse is not null then the function returns the inverse and stops
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  x$setinverse(solve(x$get(),...))
  x$getinverse()
}
