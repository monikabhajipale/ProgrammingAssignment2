# Write a pair of functions that cache the inverse of a matrix

# assume that the matrix supplied is always invertible.

# First Function
# makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) { # argument with default mode is matrix
  
  # inverseMatrix will hold value of matrix inverse which is initialized as Null
  inverseMatrix <- NULL             
  
  # set function for New Matrix
  set_Matrix <- function(n){
    # assign the new matrix in parent environment
    m <<- n
    # If new matrix then reset the inverseMatrix value as NULL
    inverseMatrix <- NULL
  }
  
  # get function for Matrix
  get_Matrix <- function() m # returns value of matrix argument
  
  # set function for inverse Matrix
  set_inverseMatrix <- function(solutionMatrix) {
    # assign the value of inverse matrix to parent enviroment
    inverseMatrix <<- solutionMatrix  
  }
  
  # get function for inverseMatrix
  get_inverseMatrix <- function() inverseMatrix # return the value of inverse matrix
  
  # order to refer the function with the $ operator
  list( 
    set_Matrix = set_Matrix,
    get_Matrix = get_Matrix,
    set_inverseMatrix = set_inverseMatrix,
    get_inverseMatrix = get_inverseMatrix
  )
}


# Second Function
# cacheSolve: computes the inverse of the special "matrix" returned by 
#             makeCacheMatrix above. If the inverse has already been calculated,
#             then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(m, ...){
  # return the matrix that is the inverse of matrix m
  inverseMatrix <- m$get_inverseMatrix() 
  
  # check the value of inverse matrix present in cache or not
  if(!is.null(inverse)){
    message("Getting Cache Value.........")
    return(inverseMatrix)
  }
  
  # getting the matrix value
  matrix_data <- m$get_matrix()
  # solving the inverse matrix using matrix_data
  inverseMatrix <- solve(matrix_data, ...)
  
  # setting the inverse Matrix
  m$set_inverseMatrix(inverseMatrix)
  # printing the inverse matrix
  inverseMatrix
}

# created by Monika Bhajipale
