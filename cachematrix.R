## This pair of functions make it possible to calculate the inverse of a matrix and cache it.
## This way, if the inverse has been previously calculated, the user can access its value in a cache

## The first function makeCacheMatrix creates a list containing four functions that 
#set the value of a the matrix, get the value of the matrix, 
#set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #initializes variable to hold inverse matrix
  i <- NULL
  
  #set function, allows user to set value of matrix, sets inverse to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get function, passes user the value of the matrix
  get <- function() x
  
  #set inverse function, allows user to set value of matrix inverse
  setinv <- function(inv) i <<- inv
  
  #get inverse function, passes user the value of the matrix inverse
  getinv <- function() i
  
  #returns a list containing the previous four functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This second function takes a list created in makeCacheMatrix and returns the value of the inverse matrix
##If the inverse has not been calculated previously, it computes the value.
##If it has, it simply accesses the cache to return the value.

cacheSolve <- function(x, ...) {
       #gets the current value of the inverse stored in x
  i <- x$getinv()
  
  #if the value of i is not null, then the inverse has been found previously
  #No calculation is needed, i is returned as is, and the funciton ends here
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  #if the previous code did not run, the inverse needs to be calculated
  #first, the original matrix is passed to a variable
  data <- x$get()
  
  #the inverse is calculated and stored in i
  i <- solve(data, ...)
  
  #i is set as the matrix inverse in x
  x$setinv(i)
  
  #i is returned
  i
}
