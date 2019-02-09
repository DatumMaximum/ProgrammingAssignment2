## cachematrix attemps to save computation time in R by making use of caching
## and lexical scoping within R. This function will perform a matrix inversion 
## if the user passes a new matrix into the function.

## use this function as follows:
##      newMatrix <- makeCacheMatrix('define_matrix')
##      cacheSolve(newMatrix)

## makeCacheMatrix will take a user defined square matrix and compute the inverse.
## it will store this inverse in the parent environment of the function 
## using the locally defined getinv and setinv as the variable m. 

makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve will check the m variable and check if m is null. 
## If m is not null the cached data will be returned. 
## If m is not null cacheSolve will solve for the inverse of the new data
## passed to the function using the setinv() function defined in makeCacheMatrix
## and return m to the user

cacheSolve <- function(x, ...) {

        m <- x$getinv()
        if (!is.null(m)){
            
            message("getting cached data")
            return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        
}
