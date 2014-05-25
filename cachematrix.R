## These functions create a matrix, computate its inverse and cache it

## Creating a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<-solve
  getinv <- function()inv
  list(set=set, get=get,
       setinv=setinv, getinv=getinv)

}

##  Computing the inverse of the special "matrix"
##(If the inverse has already been calculated, retrieves it from the cache)

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)    
  }
  data <- x$get()
  inv<-solve(data,...)
  x$setinv(inv)
}
