## 
## makeChacheMatrix - Saves matrixData and invertedMatrix data to private
## variables.  Usage:
## Creation
##   mat = matrix(1:4,2,2)
##   mm = makeCacheMatrix(mat)
##  Methods
##   mm$get() - Returns the matrix either defined in creation or by $set()
##   mm$getInvertedMatrix() - Returns the invertedMatrix variable that is defined in set()
##   mm$set(matrix)
##   mm$setInvertedMarix(solve(matrix)) - Stores the results of the solve() function.
##

## Write a short comment describing this function

makeCacheMatrix <- function(matrixData = matrix()) {
  #
  # Initialize internal variables.  Needed in case
  # the variable is accessed before the setInvertedMatrix function is called
  #
  invertedMatrix  <- NULL 
  
  #
  # set():  storesmatrixData and null to invertedMatrix.  
  #
  set <- function(inMat) {
    matrixData <<- inMat
    invertedMatrix <<- NULL
  }
  
  # Get - Return MatrixData
  get <- function() { 
    #return it
    matrixData 
  }
      
  
  #
  # insevertedMatrix receives results of solve()
  #
  setInvertedMatrix <- function(inMat) {
    invertedMatrix  <<- inMat
  }
    
  #
  # REturns the invertedMatrix variable.
  #
  getInvertedMatrix <- function() invertedMatrix
  
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)

}


## cacheSolve() - function that will either 1) Calculate and return an 
##                inverted matrix if not previously solved.  Or return 
##                a previously solved matrix (without solving)
## Parameters:
##   cacheSolve(mcm) where  mcm= a previously created makeCacheMatrix object.
##  Usage:
##  mat = matrix(1:4,2,2)
##   mm = makeCacheMatrix(mat)
##  cacheSolve(mm)
##    -> New inverted matrix created and returned
##  cacheSolve(mm)
##    -> 2nd time:  Inverted Matrix returned from memory (not solved again)
##  

cacheSolve <- function(mcm, ...) {
  # get the inverted matrix from the makeCacheMatrix object.  This 
  # returns null if not previously defined.
  mat <- mcm$getInvertedMatrix()         
  #
  # solve() has been previously defined.  Just return what is in memory.
  #
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  else {
    #
    # solve() to get inverted matrix.  And Store results.
    #
    message("SOlving and storing results in cache memory")
    data <- mcm$get()
    mat <- solve(data, ...)
    mcm$setInvertedMatrix(mat)
    return(mat)  
  }
  
}
