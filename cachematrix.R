###creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
    
    #m use as a value of the matrix to store the inverse
    m <- NULL
    
    #set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    #get the value of the matrix
    get <- function() x
    
    #set the value of the matrix inverse
    setInverse <- function(solve) m <<- solve
    
    #get the value of the matrix inverse
    getInverse <- function() m
    
    # return the special matrix
    list(set = set, get = get,setInverse = setInverse,
         getInverse = getInverse)
}

### Compute the inverse of the special "matrix" and returned by makeCacheMatrix. 
### If the inverse has already been calculated (and the matrix has not changed), 
### then cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    #get the cached inverse and stored in the variable ,called m
    m <- x$getInverse()
    
    #Check the value of the cached get the above 
    #if the inverse actually cached,print a message and return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    
    
    data <- x$get()
    #Check the matrix if it have the inverse else, calculate the inverse and cache it
    if(det(data) == 0){
        message("the maxtrix have no inverse")
    }
    else{    
        #use solve function to calculate the inverse
        m <- solve(data, ...)
        #use setInverse to cache the matrix inverse just calculate
        x$setInverse(m)
        m
    }
    
}

###Test
## case the matrix have the inverse
#> mt <- makeCacheMatrix(matrix(c(1,4,5,2,1,2,3,2,3), 3, 3)) 
#> mt$get()                                       
#->  1    1    1
#    1    1    1
#    1    1    1
#> mt$getInverse() -> Null
#> cacheSolve(mt)  
#> mt$getInverse() ->  -0.25 -1.776357e-16  0.25
#                      -0.50 -3.000000e+00  2.50
#                       0.75  2.000000e+00 -1.75
#> cacheSolve(mt)  -> "getting the data"

## case the matrix have the inverse
#> mt <- makeCacheMatrix(matrix(1:9), 3, 3)) 
#> mt$get()                                       
#->  1    2    3
#    4    5    6
#    7    8    9
#>cacheSolve(mt) -> "the maxtrix have no inverse"




