## makeCacheMatrix is a listing containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set = set, # gives the name 'set' to the set() function defined above
       get = get, # gives the name 'get' to the get() function defined above
       setinverse = setinverse, # gives the name 'setinverse' to the setinverse() function defined above
       getinverse = getinverse) # gives  the name 'getinverse' to the getinverse() function defined above
}


## The following code calculates the inversed matrix created with the above function
cacheSolve <- function(x, ...) {
  # To test if the reversal has been calculated:
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv) # return inv if the inversed matrix has been existed
  }
  # To calculate the inversed matrix if inv is empty
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
