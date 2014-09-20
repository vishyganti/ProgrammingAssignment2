## This function creates a special 'matrix' object that can create its invers
## cache it internally. Use of this object is recommended when repeated 
## inverse operations are required as it will result in better speed. However
## the storage requirements will increase.


makeCacheMatrix <- function(x = matrix()) {
  #Create the matrix that will hold the inverse
  m <- NULL
  
  #If input matrix is not a square matrix, print warning
  if (nrow(x) != ncol(x)) print("Warning: Input is not a square matrix")
  
  ## Setter and getter functions
  
  #Function to reset the original matrix
  set <- function(y) {
    #If input matrix is not a square matrix, print warning
    if (nrow(y) != ncol(y)) print("Warning: Input is not a square matrix")

    x <<- y
    m <<- NULL
  }
  
  #Function to return the original matrix
  get <- function() x
  
  #Function to set the cached inverse. The input argument must be the inverse
  # of the original matrix. Note that the actual computation
  # of the inverse is not performed here.
  setinverse <- function(t){
    
    #If input argument is not a square matrix, throw an error
    if (nrow(t) != ncol(t)) {
      print("Error - Input matrix is not square")
      return(m <<- NULL)
    }
    
    # Next, make sure that the input argument is really the inverse of X.
    # This check is required to ensure that whichever function is calling
    # this function is first creating the inverse.
    # The inverse is calculated twice every time the original matrix 
    # is reset. This reduces the efficiency of caching, but preserves 
    # the integrity of the functions.
    tmp <- solve(t)
    
    rows = nrow(tmp)
    cols = ncol(tmp)
    
    flag = 0
    if ((nrow(tmp) != nrow(x)) && (ncol(tmp) != ncol(x))) flag = 1
      
    ## Perform an element wise check of the rounded values. We have to do 
    ## this because the inverse of an inverse is not exactly returning the
    ##original matrix due to rounding errors
    if (flag == 0) {
      length <- nrow(tmp)*ncol(tmp)
      for (i in 1:length) {
       if (round((tmp[i][1])) != round((x[i][1]))) {
          flag = 1
       }
      }
    }
    
    if (flag == 1) {
      print("Error: Input argument is not inverse of original matrix")
      return (m <<- NULL) 
    }
      
    m <<- t
  } 
  
  #Function to get the stored inverse
  getinverse <- function() m
  
  #The list of functions that can be called on this object.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function retruns the inverse from the cache if it has already 
## been created.Otherwise it creates the inverse, sets the inverse inside
## the cache, and returns the inverse

cacheSolve <- function(x, ...) {
  
  ## If matrix has not changed, then return the cached inverse and 
  ## exit the function
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the function gets here, the matrix has changed. 
  # If the new matrix is not a square matrix, print message and 
  # return NULL
  data <- x$get()
  if (nrow(data) != ncol(data)) {
    print("Error - Input matrix is not square")
    return (NULL)
  }
  
  
  ## Now check if the determinant is zero
  ## If it is, then the matrix is not invertible so set m to NULL.
  ## Otherwise compute the inverse and return the inverse
  dr = det(data)
  
  if (dr != 0) {
    m <- solve(data, ...)
    m <- x$setinverse(m)
  }
  else {
    print("Error: Matrix is not invertible")
    return (m<-NULL)
  }
  m
}
