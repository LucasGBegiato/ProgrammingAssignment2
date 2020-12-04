
##This function takes the matrix that is choosen by the user and after that, you must use those two expressions
#pmatrix <- makeCacheMatrix(matrix(c(1,2,1,4),nrow=2,ncol=2))
#pmatrix$get()
#And then, you gonna get the inverse result by using that expression:
#cacheSolve(pmatrix)

#So, to start, we gonna work in a formula to take a matrix and return a data set.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#For now, we have to create a formula to make the inverse matrix, by using some variables that we used before.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

#So, we can see a application for that construction.
#For example, if we take this matrix:
pmatrix <- makeCacheMatrix(matrix(c(1,2,1,4),nrow=2,ncol=2))
pmatrix$get()
#The inverse is given by:
cacheSolve(pmatrix)

#Thank you.


