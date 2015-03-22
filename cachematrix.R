makeCacheMatrix <- function( x = matrix() ) {
  
  ## Initialize the inverse property
  m <- NULL  ## sets the value of m to Null
   set <- function( y ) {
    x <<- y
    m <<- NULL 
  }
  
  ## Method the get the matrix
 get <- function() x
      
  ##  set the inverse of the matrix
  setInverse <- function(inverse) {
   m <<- inverse
 }
  
  ##  get the inverse of the matrix
  getInverse <- function() { 
    ## Revert the inverse property
    m
 }
  ## Back a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Compute the inverse of the unique matrix back by "makeCacheMatrix"
cacheSolve <-function(x, ...){ 
## Back to a matrix  "m"
m <- x$getInverse() ## if an inverse has already been calculated m gets it
if(!is.null(m)) { ##see if cachesolve has run before
  message("get the cached data")
  return(m)
}
## Get the matrix used by makeCacheMatrix function
data <- x$get()
## Compute the inverse via matrix multiplication
m <- solve(data, ...) 
## Set the inverse to the object
x$setInverse(m)
}



