## Functions are able to cache time-consuming computation of calculating a matrix's inverse

## 1. Set the value of the matrix, 2. Get the value of the matrix, 3. Set the value of the inverse, 
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  setinverse<-function(matrix) m<<-matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }


## Calculates the inverse of the matrix created with the above function. First checks to see if the
## inverse has already been calculated.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  } 
