
## makeCacheMatrix creates a special "matrix" with a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix  

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y){
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)  
}


## The cacheSolve function calculates the inverse of a matrix. If the
## inverse has already been calculated it returns it from the cache,
## otherwise it calculates the inverse, returns it and stores in cache
## for future reference

cacheSolve <- function(x, ...) {

    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
