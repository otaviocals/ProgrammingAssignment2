##			R Programming	-	Coursera
##				Programing Assignment 2
##					07/2015



## This function creates an object that stores on cache a matrix, it's 
## inverse (if already calculated) and methods to both read and write
## the stored matrix (get and set) and inverse matrix (getimatrix and
## setimatrix).
## It's input is the matrix to be stored and it returns a list of it's
## methods.

makeCacheMatrix <- function(x=matrix()){

	im <- NULL			## Initialize the inverse matrix as 
					## NULL (uncalculated).

	set <- function(y){		## Method to replace the stored 
			x<<- y		## matrix. It resets the stored
			im<<- NULL	## inverse matrix value to NULL.
		}

	get <- function() x		## Method to read the stored matrix.

	setimatrix <- function(imatrix) im<<-imatrix    ## Method to replace
				        	        ## the stored inverse
							## matrix.

	getimatrix <- function() im	## Method to read the stored inverse
					## matrix.

	list(set=set,get=get,setimatrix=setimatrix,getimatrix=getimatrix)
					## Returns it's methods as a list.
}

## This function receives the object created by makeCacheMatrix and
## calculates it's stored matrix's inverse, if still uncalculated, or
## returns it's stored inverse matrix if already previously calculated.

cacheSolve <- function(x,...){

	im <- x$getimatrix()				## Reads the stored
							## inverse matrix.

	if(!is.null(im)){				## If the inverse matrix
		message("getting cached data")	        ## has been calculated
		return(im)				## already, returns it's
	}						## stored value.

	data <- x$get()		        	## Reads the stored matrix.

	im <- solve(data,...)		        ## Calculates the inverse matrix.

	x$setimatrix(im)			## Stores the inverse matrix on the
						## object cache.

	im					## Returns the calculated inverse
						## matrix.
}
