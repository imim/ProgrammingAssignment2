# AUTHOR: imim@tid.es

# PURPOSE: R functions, able to cache potentially time-consuming computations,
#          applied to obtain the inverse of a matrix using "solve".
#          If the contents of a matrix are not changing, it may make sense to
#          cache the value of the inverse matrix, so that when we need it again,
#          it can be looked up in the cache rather than recomputed. 
#          We will take advantage of the scoping rules of the R language and how
#          they can be manipulated to preserve state inside of an R object.

#          makeCacheMatrix: This function creates a special "matrix" object that
#          can cache its inverse.

#          cacheSolve: This function computes the inverse of the special
#          "matrix" returned by makeCacheMatrix above. If the inverse has
#          already been calculated (and the matrix has not changed), then the
#          cachesolve should retrieve the inverse from the cache.

# LIMITATIONS: Computing the inverse of a square matrix will be done with the 
#              solve function in R. For example, if X is a square invertible
#              matrix, then solve(X) returns its inverse.
#              We are assumming that the matrix supplied is always invertible.

# DATE: June, 16th, 2014

# EXECUTION EXAMPLES:
# source("week_3_assignment.R")
# > inputMatrix <- matrix (1:4, 2, 2)
# > inputMatrix
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > exMatrix <- makeCacheMatrix(inputMatrix)
# > cacheSolve(exMatrix)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(exMatrix)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# > inputMatrix2 <- matrix (c(1,0,-1,0,2,0,1,0,1), 3, 3)
# > solve(inputMatrix2)
#      [,1] [,2] [,3]
# [1,]  0.5  0.0 -0.5
# [2,]  0.0  0.5  0.0
# [3,]  0.5  0.0  0.5
# > exMatrix2 <- makeCacheMatrix(inputMatrix2)
# > cacheSolve(exMatrix2)
#      [,1] [,2] [,3]
# [1,]  0.5  0.0 -0.5
# [2,]  0.0  0.5  0.0
# [3,]  0.5  0.0  0.5
# > cacheSolve(exMatrix2)
# getting cached data
#      [,1] [,2] [,3]
# [1,]  0.5  0.0 -0.5
# [2,]  0.0  0.5  0.0
# [3,]  0.5  0.0  0.5


# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse. (Execution examples, above, in header comments).
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache, and a message "getting cached data" is printed. 
# (Execution examples, above, in header comments).
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}