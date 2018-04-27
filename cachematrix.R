## makeCacheMatrix is used to create a special matrix object that can cache its inverse
## cacheSolve computes the inverse of the special matrix returned from above function and stores
## it in cahce. But, if the inverse already exists, then it is retrived from the cache

## This function returns a list of functions get,set,getInv,setInv

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function takes the matrix and computes the inverse or retrives it from cache
## if it has already been computed

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
