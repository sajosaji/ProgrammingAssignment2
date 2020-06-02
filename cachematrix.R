

##makeCacheMatrix is used to cache the matrix 
##so that same matrix dont have to be solved again and again

makeCacheMatrix <- function(x = matrix()) {
         i<- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)

}


## cacheSolve to get the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
}
