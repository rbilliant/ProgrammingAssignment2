## The functions allow you to cache the inverse of an invertible square 
## matrix and attribute it to the matrix itself. Once calculated, it can
## just be read out of the cache such that possibly intensive calculations
## can be avoided.


# The function makeCacheMatrix enables us to create a new matrix object
# in which we can attribute the matrix inverse to an ordinary matrix object.
# The output is a list of functions that can be used within the makeCacheMatrix
# object.

makeCacheMatrix <- function(x = matrix()) {
     
     ordinary_matrix <- x 
     inverse <- NULL
     
     set_matrix <- function(some_matrix) {
          ordinary_matrix <<- some_matrix # set the matrix in the parent environment (the one of function makeCacheMatrix)
          inverse <<- NULL # reset the assigned inverse for the case a different matrix has been set
     }
     get_matrix <- function() ordinary_matrix # to be able to output the matrix
     
     set_inverse <- function(matrix_corresp_inverse) { 
          inverse <<- matrix_corresp_inverse
     } # allows us to attribute the matrix inverse to our "CacheMatrix".
     # The ordinary matrix does not possess this attribute.
     
     get_inverse <- function() inverse # to be able to output the inverse
     
     list(set_matrix = set_matrix,
          get_matrix = get_matrix,
          set_inverse = set_inverse,
          get_inverse = get_inverse) # the list that is being output
}




# The function cacheSolve yields the inverse of the matrix input.
# Case 1: it uses the get_matrix functionality of the makeCacheMatrix object
# to just read out the inverse if available.
# Case 2: it uses the function "solve" to calculate the inverse if the inverse
# is not available in the makeCacheMatrix object "mat". After the calculation
# the inverse is cached in the makeCacheMatrix object.
# The output is the inverse of the input matrix

cacheSolve <- function(mat, ...) {
     # inverse might be available in the cache
     # of the makeCacheMatrix object mat
     if(!is.null(mat$get_inverse()) == TRUE) { 
     
          message("inverse in cache...reading cache")
          inverse <- mat$get_inverse()
          
     } else {
          
          message("inverse not in cache: calculating inverse using solve()...")
          inverse <- solve(mat$get_matrix(), ...) # inverse needs to be calculated using solve()
          mat$set_inverse(inverse) # once it is calculated, it is clever to cache the inverse
          
     }
     inverse
}