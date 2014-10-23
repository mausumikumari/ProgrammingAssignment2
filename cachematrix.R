##Does caching of inverse of the matrix. If inverse is not found,calculates the inverse and caches the inverse .

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {


	inverse<-NULL

##seta the matrix object
	set<- function(matrix){

	x<<-matrix
	inverse<<-NULL

	}

##to get the matrix
	get<-function(){
		x
	}


## set the inverse for the matrix
	setInverse<-function(i){
		inverse<<-i
	}

## get the inverse for the matrix	
	getInverse<-function(){
		inverse
	}

	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)



}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


		inv<-x$getInverse()


	## checks whethwe inverse is present in the list
		if(!is.null(inv))
		{
			message("Getting cached data!")
			return(inv)
		}

	## if no inverse , calculate inverse using solve() function

		matrix<-x$get()

		inv<-solve(matrix)

	## inverse is set in the list	using setInverse function.	

		x$setInverse(inv)

		inv
}