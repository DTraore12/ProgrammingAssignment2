##  The goal of this assignment is to write a pair of functions that cache the inverse of a matrix.Put comments here that give an overall description of what your
## functions do

## Following the same steps in example provided in the assignement, but instead replaced "vector" with CacheMatrix and "mean" with solve
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
get = get,
setInverse = setInverse,
getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}
mat <- x$get()
inv <- solve(mat, ...)
x$setInverse(inv)
inv
}
## Testing the fuctions provide the following result. 
> source("C:\\Users\\dalen.traore\\Downloads\\CourseraHopkins.R\\Test3.R")
> test_matrix <- makeCacheMatrix(matrix(c(1:6,3, 1, 8), 3, 3))
> test_matrix$get()
     [,1] [,2] [,3]
[1,]    1    4    3
[2,]    2    5    1
[3,]    3    6    8
> test_matrix$getInverse()
NULL
> cacheSolve(test_matrix)
           [,1]        [,2]       [,3]
[1,] -1.2592593  0.51851852  0.4074074
[2,]  0.4814815  0.03703704 -0.1851852
[3,]  0.1111111 -0.22222222  0.1111111
> cacheSolve(test_matrix)
getting cached data
           [,1]        [,2]       [,3]
[1,] -1.2592593  0.51851852  0.4074074
[2,]  0.4814815  0.03703704 -0.1851852
[3,]  0.1111111 -0.22222222  0.1111111
test_matrix$getInverse()
           [,1]        [,2]       [,3]
[1,] -1.2592593  0.51851852  0.4074074
[2,]  0.4814815  0.03703704 -0.1851852
[3,]  0.1111111 -0.22222222  0.1111111
