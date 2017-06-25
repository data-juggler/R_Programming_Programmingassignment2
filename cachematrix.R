## makeCacheMatrix is the function to create the list containing following functions:
## 1. set the Matrix
## 2. get the Matrix 
## 3. set the inverse Matrix
## 4. get the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        set = function(y){
                x <<- y
                i <<- NULL
        }
        get = function() x
        setinverse = function(inverse) i <<- inverse
        getinverse = function() i
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
        i = x$getinverse()
        
        # if inverse Matrix is already there, show it
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        # if inverse Matrix hasn't been been calculated yet, calculate it:
        thematrix = x$get()
        i = solve(thematrix, ...)
        
        # Put it into the cache and show inversed Matrix
        x$setinverse(i)
        i
       
}


