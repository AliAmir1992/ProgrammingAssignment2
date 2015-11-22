
##  This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
	invX <- NULL
	set <- function(y) {
			x <<- y
			invX <<- NULL
	}
	get <- function() x
	setInv <- function(inv) invX <<- inv
	
	getInv <- function() {
		if(!is.null(invX)){
			message("getting cached data")
			return(invX);
		}
		invX <<- solve(x)
		setInv(invX)
		invX
	}
	list(set = set, get = get,
		 setInv = setInv,
		 getInv = getInv)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invX <- x$getInv()
		invX
}