## The code will calculate the inverse matrix. The inverse matrix will only be
## calculated if it has not been calculated before

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Creates a NULL object
        i <- NULL 
        
        # Sets the value of the matrix
        setMatrix <- function(y) { 
                x <<- y
                i <<- NULL
        }
        
        # Gets the value of the matrix
        getMatrix <- function() x
        
        # Sets the value of the inverse matrix
        setInverseMatrix <- function(inverse) i <<- inverse 
        
        # Gets the value of the inverse matrix
        getInverseMatrix <- function() i 
        
        # Creates a list comprising matrix values and inverse matrix values
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix, but only if it
## has not been computed already

cacheSolve <- function(x, ...) {
        
        # Returns a matrix that is the inverse of x
        i <- x$getInverseMatrix() 
        
        # Displays a message if the matrix is already stored in the cache, and then returns 
        # the cached computation
        if(!is.null(i)) {  
                message("getting cached data")
                return(i)
        }
        
        # Calculates the matrix from the data
        data <- x$getMatrix()
        
        # Stores calculation of solve funciton in i
        i <- solve(data)
        
        # Returns the inverted matrix
        x$setInverseMatrix(i)
        i
}
