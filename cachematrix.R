## Caching - Assignment2 of Coursera course in R Programming
## This R file contains two functions 
## 1.makeCacheMatrix: This function creates a special square matrix object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" 
##               returned by makeCacheMatrix function. 
##               If the inverse has already been calculated (and the matrix has not changed), 
##               then the cachesolve retrieves the inverse from the cache.
## ------------------------------------------------------------------------------------------------------




## The function, makeCacheMatrix creates a special square "matrix" since solve only handles square matrix. 
## This function does the following 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
  
  ## initially there is nothing cached, so this is set to NULL
  
  cacheInverse <- NULL
  
  ## Setting a new value for the matrix and flushing cache off the old inverse value
  set <- function (newVal) {
    
    x <<- newVal
    
    cacheInverse <<- NULL
  }
  
  ## Returns the stored original matrix
  get <- function (){
    
    x
    
  } 
  
  ## Set the solved inverse and cache it
  setInverse <- function(solve) {
    
    cacheInverse <<- solve
    
  }
  
  getInverse <- function() cacheInverse
  
  ## Each element in the list is a function
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


## The function cacheSolve calculates the inverse of the special "matrix"  
## However, it first checks to see if the inverse matrix has already been 
## calculated. If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix and sets the value of the inverse matrix in the 
## cache via the setCache function.

cacheSolve <- function(x, ...) {
  
  ## Get the cached inverse matrix  
  invMatrix <- x$getInverse()
  
  ## If the cached matrix exists and is not NULL, return it
  if(!is.null(invMatrix)){
    
    message("getting cached inverse matrix")
    return (invMatrix)
  }
  
  ## Else, get the original matrix
  orgMatrix <- x$get()
  
  ## Solve and get the inverse matrix
  invMatrix <- solve(orgMatrix)
  
  ## Set the inverse matrix to cache
  x$setInverse(invMatrix)
  
  ## return the inverse matrix
  invMatrix
        
}
