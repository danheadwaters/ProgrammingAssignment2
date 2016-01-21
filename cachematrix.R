## The first function, `makeCacheMatrix` creates a special "vector", which is
## really a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
     
     ## Create the inverse matrix, my_mat.  
     ## Initially filled with NA
     my_mat <- matrix()
     
     ## the set function allows the x matrix and inverse matrix to
     ## be reset.
     set <- function(y = matrix()) {
          x <<- y
          my_mat <- matrix()
     }
     get <- function() x
     getinverse <- function() my_mat
     
     ## Assumes that the inverse is set in cacheSolve function.
     setinverse <- function(inverse = matrix()) my_mat <<- inverse
     list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## mean has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the data, actually a matrix, and sets the value of the inverse in the 
## cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     
     my_mat <- x$getinverse()
     if(!all(is.na(my_mat))) {
          message("getting cached data")
          return(my_mat)
     }
     data <- x$get()
     my_mat <- solve(data)
     x$setinverse(my_mat)
     my_mat

}
