## Here are functions which help to cache the inverse of a matrix 
## instead of computing it every time from scratch.

## makeCacheMatrix makes a list of functions which:
## 1. Return the matrix for which we want to compute the inverse matrix.
## 2. Set the variable in which we want to store the inverse matrix.
## 3. Get the value of the variable which stores the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  data<-0
  Inverse<-matrix(data)
  getMatrix <- function() {x}
  setInverse <- function(inv) {Inverse <<- inv}
  getInverse <- function() {Inverse}
  list(getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSolve checks if there is a non-zero matrix stored
## in the variable where we want to store the inverse matrix.
## If so, the function gets the value from this variable.
## If not, the function sets the value of this variable by computing
## the inverse matrix using function 'solve'.
## I decided to mark the inverse matrix by zero matrix if this inverse
## has never been computed yet - so I've written additional function
## 'is.zero.square' which returns TRUE if the square matrix is a zero
## matrix and FALSE if it is not. It's probably not necessary but 
## I had some problems and it was the quickest way to deal with them. 

cacheSolve <- function(x, ...) {
  Inverse <- x$getInverse()
  is.zero.square<-function(y=matrix()){
    n<-nrow(y)
    for (i in 1:n){
      for(j in 1:n){
        if(y[i,j]!=0)
          return(FALSE)
      }
    }
    TRUE
  }
  if(!is.zero.square(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$getMatrix()
  Inverse <- solve(data, ...)
  x$setInverse(Inverse)
  Inverse
  
}
