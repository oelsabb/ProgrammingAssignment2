#makecachematrix: caches the values of the matrix and matrix's inverse
# and provides global accessors to these

makecachematrix <- function(x = matrix()){
  ##initialize m (locally)
  m <- NULL
  ##setter for the "value" of the matrix
  ##In this case the mean needs to be reset
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## getter for the "value" of the matrix
  get <- function() x
  
  ##getter for the inversed matrix
  getsolve <- function() m
  
  ##setter for the inversed matrix 
  setsolve <- function(solve) m <<- solve
  
  ##a list of accessors for the functions defined in 'makecachematrix'
  list(get=get, set=set, getsolve=getsolve, setsolve=setsolve)
}


## cachesolve: returns the inverse of the matrix (if cached). 
## If not, creates the cache, stores it (for future use) and returns it  

cacheSolve <- function(x,...){
  #gets the matrix x's inverse (if exists)
  m <- x$getsolve()
  ##checks whether the inverse of is cached
  if(!is.null(m)){
    print("getting the inverse of the matrix")
    ## returns the value of m
    return (m)
  }
  
  ##stores the matrix into the variable 'mymatrix'
  mymatrix <- x$get()
  
  ## doing the inverse operation for the matrix
  m <- solve(mymatrix, ...)
  ## storing the inverse in the cache 
  x$setsolve(m)
  ##returning the inverse
  m

  
}
