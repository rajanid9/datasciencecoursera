## Caching the inverse of a matrix
## Assumption: Matrix supplied as input is invertible (square matrix)

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## Returns a list containing functions set(), get(), setinv(), getinv()
## x is passed as a function argument and defined as a matrix

makeCacheMatrix <- function(x = matrix()) 
{
## Initializing inv as an object to be used later
    inv <- NULL
    
## Functions to retrieve and set data values
## Using '<<-' to assign values to an object in the parent environment
    
    set <- function(y) 
    {
## Assigning input argument y to object x in parent environment
        x <<- y
## Assigning null value to inv object in parent environment
        inv <<- NULL
## Whenever x is reset, value of inv cached in memory of the object is cleared
    }
    
## Function to retrieve x value from parent environment
    get <- function() x
    
## Assigning inverse to the value of object inv in parent environment
    
    setinv <- function(inverse) inv <<- inverse
    
## Gets the object inv
    
    getinv <- function() inv
    
## Assigning the above functions as a named element in a list
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve function computes the inverse of the "special" matrix returned by makeCacheMatrix
## If the inverse has already been calculated(and the matrix has not changed) 
##    then cacheSolve uses the cache to retrieve the inverse

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
## Logic to check if the inverse has already been calculated
    
    if(!is.null(inv)) 
    {
## Gets the inverse from cache and skips computing 
        message("getting cached data")
        
## Returns the inverse of the original matrix input x
        return(inv)
    }
## Else compute the inverse of the matrix
    
    data <- x$get()
    
## Use the solve function to compute the inverse
    
    inv <- solve(data, ...)
    
## Set the value of inverse in cache
    
    x$setinv(inv)
    
## Return the inverse
    inv
}

    
    
## Testing the functions
## a <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(a)
##      [,1] [,2]
## [1,]  -2  1.5
## [2,]   1 -0.5
## cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]  -2  1.5
## [2,]   1 -0.5

