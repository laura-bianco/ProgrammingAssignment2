## This function takes a matrix x and caches its inverse matrix

## The makeCacheMatrix function creates a special matrix object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
     inverse_matrix <- NULL
     
     set <- function(newMatrix) {
          x <<- newMatrix
          inverse_matrix <<- NULL
     }
     
     get <- function() x
     
     set_inverse_matrix <- function(inverseMatrix) {
          inverse_matrix <<- inverseMatrix
     }
     
     get_inverse_matrix <- function() inverse_matrix
     
     list(set = set, get = get, set_inverse_matrix = set_inverse_matrix, get_inverse_matrix = get_inverse_matrix)
}


## This function calculates the inverse of the matrix returned by the function above
## if the inverse is already calculated, then it retrives the stored information

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse_matrix <- x$get_inverse_matrix()
     
     if (!is.null(inverse_matrix)) {
          message("Getting cached data")
          return(inverse_matrix)
     }
     
     matrix <- x$get()  
     inverse_matrix <- solve(matrix)
     x$set_inverse_matrix(inverse_matrix)  
     inverse_matrix
}
