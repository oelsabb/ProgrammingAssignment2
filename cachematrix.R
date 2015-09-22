## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


##My short comment 

makecachematrix <- function(x = matrix()){
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getsolve <- function() m
  setsolve <- function(solve) m <<- solve
  
  list(get=get, set=set, getsolve=getsolve, setsolve=setsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x,...){
  m <- x$getsolve()
  if(!is.null(m)){
    print("getting the inverse of the matrix")
    return (m)
  }
  mymatrix <- x$get()
  m <- solve(mymatrix, ...)
  x$setsolve(m)
  m

  
}
