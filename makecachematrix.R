## Put comments here that give an overall description of what your
## get()=gives cached matrix
##getInverse()gives the inverse 

## getting a cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<-y
      inv <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse){inv <<-inverse}
    getInverse <-function(){inv}
    list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
  }


## getting inverse of cached matrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)      ## Return a matrix that is the inverse of 'x'
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
       
