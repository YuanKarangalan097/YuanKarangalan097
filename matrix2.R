##I'll set the x as a matrix
##and then set the matrix columns and rows
##after that I'll set the inv as null
makeCacheMatrix <- function(x = matrix(1:4, nrow = 2, ncol = 2)) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {x}
    #' I'll set the inverse here
	setInverse <- function(inverse) {inv <<- inverse}
    #' then get the inverse here
	getInverse <- function() {inv}
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cachesolve <- function(x, ...){
	inv <- x$getInverse()
	if(!is.null(inv)){
		message("starting to get the cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}