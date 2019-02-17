Put comments here that give an overall description of what your
## functions do
#cachematrix.R ---> Rahul Aggarwal

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ##This function makeCacheMatrix gets a matrix as an input, sets the value of the matrix,
    #get the value of the matrix, set the inverse Matrix and get the inverse Matrix.
    
    #take the matrix as an input
    makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
            x <<- y
            invMatrix <<- NULL
        }
        
        getMatrix <- function() x                              #get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse  #set the value of the invertible matrix
        getInverse <- function() invMatrix                     #get the value of the invertible matrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse, getInverse = getInverse)
        
    }
    
    
    ## Write a short comment describing this function
    cacheSolve <- function(x, ...) {
        ## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an input and checks whether inverse matrix has any values in it or not.
        # In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from and set the invertible  matrix by using the solve function.
        # In case inverse matrix from has some value in it, it returns a message  "Getting Cached Invertible Matrix" and the cached object
        
        
        
        cacheSolve <- function(x, ...) {
            ## Return a matrix that is the inverse of 'x'
            
            #get the value of the invertible matrix from the makeCacheMatrix function
            invMatrix <- x$getInverse()
            if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix
                return(invMatrix)
            }
            
            #if value of the invertible matrix is NULL then
            MatrixData <- x$getMatrix()
            invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
            x$setInverse(invMatrix)                         #set the invertible matrix
            return(invMatrix)
        }

