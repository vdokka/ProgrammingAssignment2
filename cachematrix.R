# The function "makeCacheMatrix" creates a list that contains 4 member functions: 
# set, get, setInv and getInv. 
# This function uses the <<- operator to assign a value to an 
# object in an environment that is different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
   x_inv <- NULL 
   # To store the result of Matrix Inversion
   # set() is used to set a matrix to object created 
   # by makeCacheMatrix()
		         
   set <- function(y) {
     x <<- y
     x_inv <<- NULL 
   # Initialize the x_inv to NULL in an environment that is 
   # different from the current environment
    }
   get <- function() x 
   # RETURN the input matrix
   setInv <- function(inv) x_inv <<- inv 
   # SET the inverse matrix
   getInv <- function() x_inv 
   # RETURN the inverse matrix
   list(set = set, get = get,
   setInv = setInv,
   getInv = getInv)
   # RETURN a list that contains all the above defined functions
   # Sample usage of makeCacheMatrix() can be defined as follows:
   # x <- makeCacheMatrix(input_matrix)
   # x$set(new_matrix) # to ASSIGN a new matrix
   # x$get    # to get the ASSIGNED matrix
   # x$setInv # to SET the inverse matrix
   # x$getInv # to GET the inverse matrix
}

## The function cacheSolve fetches the Matrix Inverse if its available
## , otherwise it computes the Matrix Inverse and returns the same
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m_inv <- x$getInv() # To get the INVERSE MATRIX from object x
   if(!is.null(m_inv)) { 
   # To check if the INVERSE MATRIX is already available
   message("Fetching CACHED data")
   
   return(m_inv) # RETURN the calculated INVERSE MATRIX
   }
   data <- x$get() # If inverse matrix is not available, 
   # we use the x$get() to get the matrix object
   m_inv <- solve(data) # To generate the INVERSE MATRIX
   x$setInv(m_inv) # ASSIGN the inverse matrix obtained in 
   # the above step to the object
   m_inv # RETURN the solved(INVERSE MATRIX) final result
 }

   # Please follow the below steps to test the 2 new 
   # functions makeCacheMatrix and cacheSolve
   # To GENERATE a random square, non-singular matrix of dimension 3 X 3
   testMatrix <- matrix(runif(9,100,1000),3,3)
   # To GENERATE the makeCacheMatrix object using the "testMatrix"
   testMatrixCached <- makeCacheMatrix(testMatrix)
   # To generate or fetch calculated inverse matrix 
   # using the cacheSolve()
   testMatrixInv <- cacheSolve(testMatrixCached)

