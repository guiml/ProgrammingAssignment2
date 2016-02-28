## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#invertedm is where the result (inveted matrix) is stored. 
#Everytime makeCacheMatrix is called, this string is set to null
#This function has 4 functions in it: 
# - set, to set the matrix to be inverted in this function
# - get, to retrieve the matrix to be inverted in this function
# - setinv, to store inverted matrix values (invertedm)
# - getinv, to retrieve the invertedmatrix (invertedm) 

makeCacheMatrix <- function(x = matrix()) {
    invertedm <- NULL
    set <- function(y) {
        x <<- y
        invertedm <<- NULL
    }
    get <- function() x
    setinv <- function(solve) invertedm <<- solve
    getinv <- function() invertedm
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#This function has 2 possible outcomes based on a check:
# CHECK if there is anything on cache (if invertedm is not null)
# CASE YES (not NULL) prints out "getting.." and return the stored value by calling getinv
# CASE NO (it is NULL) process the value given and store in the cache using setinv


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invertedm <- x$getinv()
    if(!is.null(invertedm)) {
        message("getting cached data")
        return(invertedm)
    }
    data <- x$get()
    invertedm <- solve(data, ...)
    x$setinv(invertedm)
    invertedm
}
