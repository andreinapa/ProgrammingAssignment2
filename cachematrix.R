## Matrix inversion is usually a costly computation. 
## Instead of computing the inverse repeatedly, the functions below cache the inverse of the matrix 
## to save this costly computation when possible.

## makeCacheMatrix inputs a matrix and sets up the objects to be able to cache the inverse matrix

makeCacheMatrix <- function(x = matrix()) { #input x will be a matrix
    
    i <- NULL    #  i is the inverse matrix placeholder and is reset to NULL every 
                 #  time makeCacheMatrix is called

    # The next three functions are defined when makeCacheMatrix is called (but not run)
    # the functions are used by cacheSolve() to get values for x & i, 
    # and for setting the inverse matrix. 
    
    get <- function() { x }   # this function returns the value of the original matrix, x
   
    setinverse <- function(inverse)  {i <<- inverse}
    # this is called by cacheSolve() during the first cacheSolve() for this matrix
    # access and it will store the value using superassignment

    getinverse <- function() { i } # this will return the cached value to cacheSeolve() on subsequent accesses
    list(get = get,                #  This is accessed each time makeCacheMatrix() is called, making a new object      
         setinverse = setinverse,  #  It is a list of the internal functions ('methods') so a calling function
         getinverse = getinverse)  #  knows how to access those methods.                            
  }

## cacheSolve returns a matrix that is the inverse of 'x', where the input x is an object created by makeCacheMartix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()               # accesses the object 'x' and gets the inverse matrix
    if(!is.null(i)) {                 # if 'i' is not NULL, then is was alredy calcuated and cached
      message("getting cached data")  # in which case, the message is sent to the console
      return(i)                       # and the cached inverse matrix 'i' is retuned
    }
    data <- x$get()        # if 'i' is NULL, then the inverse matrix for 'x' has not yet been calcuated
    i <- solve(data, ...)  # so, the inverse matrix is calcuated using the "solve" function, and stored in 'i'
    x$setinverse(i)        # it's also stored in the object 'x'
    i                      # the newly calcuated inverse matrix 'i' is returned
  }
