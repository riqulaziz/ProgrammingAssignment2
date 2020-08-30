## this function use to creates special "matrix" that can know its inverse
## x is matrix object that user will use on the console 


makeChaceMatrix <- function(x=matrix){
  inver <- NULL
  set <- function(y){
    x<<- y
    inver <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inver
  
  list(set = set, get = get , 
       setinverse = setinverse, 
       getinverse = getinverse)
  
  
}

## this functions use to inverse special "matrix"
## if matrix has already calculate( with matrix has no change)
## its should retrieve inverse from the cache 

cacheSolve <- function(x, ...){
  inver <-x$getinverse()
  if (! is.null(inver)){
    message('Getting cached data ')
    return(inver)
  }
  mat <- x$get()
  inver<- solve(mat, ...)
  x$setinverse(inver)
  inver
  
}