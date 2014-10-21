## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {


	inverse<-NULL

	set<- function(matrix){

	x<<-matrix
	inverse<<-NULL

	}

	get<-function(){
		x
	}

	setInverse<-function(i){
		inverse<<-i
	}
	
	getInverse<-function(){
		inverse
	}

	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)



}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


		inv<-x$getInverse()

		if(!is.null(inv))
		{
			message("Getting cached data!")
			return(inv)
		}


		matrix<-x$get()

		inv<-solve(matrix)

		x$setInverse(inv)

		inv
}
