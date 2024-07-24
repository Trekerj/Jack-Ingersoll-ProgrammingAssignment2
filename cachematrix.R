## The makeCasheMatrix() and the casheSolve make use of R scoping rules to take in
## a matrix and calculate it's inverse cashing the values for later use. This avoids
## having to recalculate the inverse, an operation that can be time consuming for 
# large matrices. 

## The 'makeCasheMatrix()' will takes a square invertable matrix as input 
## It creates sub-functions, at the parent environmental level, via the <<- operator
## The 'makeCasheMatrix()' function creates a 'closure' that allows the sub functions to
## be carried into an object that is created from running the makeCasheMatrix() function
##(I've included an example below). 

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## The casheSolve() function uses the solve function to find the inverse of the matrix 
## input into the makeCasheMatrix() function.
## This function also returns the 's' variable for use in the getsolve() function

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
make_C_matrix_obj <- makeCacheMatrix(matrix(c(1,2,-1,2,1,2,-1,2,1), nrow = 3, byrow = TRUE))
invertMatrix <- cacheSolve(make_C_matrix_obj)
# view make_C_matrix_obj to see the sub functions and environments

(x_value <- make_C_matrix_obj$get())
(s_value <- make_C_matrix_obj$getsolve())

