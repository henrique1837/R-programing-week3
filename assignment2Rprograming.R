#function makeCacheMatrix
makeCacheMatrix <- function(x = numeric()) {
  
  
  cache <- NULL
  
  # store the matrix
  setMatrix <- function(novoValor) {
    x <<- novoValor
    cache <<- NULL
  }
  
  # returns stored matrix
  getMatrix <- function() {
    x
  }
  
  cacheInverse <- function(k) {
    cache <<- k
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# function cacheSolve: calculate inverse of the matrix
cacheSolve <- function(y, ...) {
  inverse <- y$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }

  
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
 
  inverse
}

#testing function cacheSolve:

var <- makeCacheMatrix()
var$setMatrix(matrixc(3,4,13,14), nrow=2,ncol=2))
var$getMatrix()
      [,1] [,2]
[1,]    3   13
[2,]    4   14

cacheSolve(var)
     [,1] [,2]
[1,] -1.4  1.3
[2,]  0.4 -0.3

 cacheSolve(var)
getting cached data
     [,1] [,2]
[1,] -1.4  1.3
[2,]  0.4 -0.3