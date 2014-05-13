## Caches the inverse of a matrix (if such exists) or else computes it

## makeCacheMatrix takes as input a matrix and creates a special "matrix" object  ...
## that can cache its inverse and then returns a list with 4 list items, i.e functions set, ...
## get,setinverse,getinverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {    #set value of matrix
        x <<- y             
        m <<- NULL
    }
    get <- function() x     #get value of matrix
    setinverse <- function(solve) m <<- solve     #set value of inverse of the matrix
    getinverse <- function() m                    #get value of inverse of the matrix 
    list(set = set, get = get,     #return a list with 4 list items
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the input matrix either by retrieving from cache if it
## exist there or by calculating it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()                #checks if its already computed
    if(!is.null(m)) {                  #returns inverse if it already exists
        message("getting cached data")
        return(m)
    }
    data <- x$get()                    #calculates if it does not exist
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}
