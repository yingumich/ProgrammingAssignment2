## Put comments here that give an overall description of what your
## functions do

## Create a special"makeCacheMatrix"matrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inver <- NULL
	set <- function(y){
		x <<- y
		inver <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inver <<- inverse
	getinverse <- function() inver
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Compute the inverse of the special"matrix"returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
	inver <- x$getinverse()
	if(!is.null(inver)){
		message("get cached data.")
		return(inver)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inver)
	inver
        ## Return a matrix that is the inverse of 'x'
}
