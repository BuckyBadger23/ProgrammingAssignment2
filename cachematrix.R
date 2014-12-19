## The function makeCacheMatrix() is passed a matrix which creates an object of type list. This object stores 
## two things, the original matrix and the cached value (the inverse matrix of the passed matrix) 
## which is initially set to "NULL"


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y)
{
x <<- y
m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The function cacheSolve() accesses the object created in makeCacheMatrix() and if the inverse matrix for the object (a matrix)
## has not been determined it is then "solved" using the solve() function. If the inverse matrix
## has already been determined earlier then the cacheSolve() function returns it directly (thus avoiding the need for solving it again).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)

}
