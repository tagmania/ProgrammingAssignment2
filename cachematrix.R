
## Functions makeCacheMatrix and cacheSolve needed for the programming assignments 
##     from the R Programming course on coursera. 
## Use example (being m an invertible matrix)
##      tmp = makeCacheMatrix(m)
##      cacheSolve(temp)



## makeCacheMatrix function definition:
##  Input
##		x --> an invertible matrix
##  Output
##		set --> set the matrix
##		get --> get the matrix
## 		setinv --> set the inverse matrix
##		getinv --> get the inverse matrix
##
## The output is used as input to cacheSolve()
##
makeCacheMatrix <- function(x = matrix()) {
              
        inv = NULL
        set = function(y) {                
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## cacheSolve function definition
##  Input
##		x --> the functions returned by the function makecacheMatrix()
##  Output
##		inverse of the original matrix (input to makeCacheMatrix())
cacheSolve <- function(x, ...) {
        
        inv = x$getinv()
        
        # here is the magic!
		# if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and avoid repeating the calculation.                 
                return(inv)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinv(inv)
        
        return(inv)
}