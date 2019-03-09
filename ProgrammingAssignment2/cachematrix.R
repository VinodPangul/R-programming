## Author: Vinod P
## This function set helps calculating the inverse of the matrix with catching mechanism in lexical ##scoping


## This function provides the setters/getters functions() for
## inverse of matrix function call

makeCacheMatrix <- function(x = matrix()) {
	inv_value<-NULL
	set<-function(value){
		x<<-value
		inv_value<<-NULL
	}
	
	get<-function(){
		x
	}
	
	setinverse<-function(l_inv_value){
		inv_value<<-l_inv_value
	}
	
	getinverse<-function(){
		inv_value
	}
	list(set=set, get=get, 
		 setinverse=setinverse,
		 getinverse=getinverse)
}


## This function uses the functions defines above to find the inverse of the mextrix 
## However, it's uses catching information for given matrix to save time/compute power

cacheSolve <- function(x, ...) {
		message('getting cached inverse data')
        ## Return a matrix that is the inverse of 'x'
        inv_val<-x$getinverse()
        if(!is.null(inv_val)){
        	message('getting cached inverse data')
        	return(inv_val)
        }
        
        data<-x$get()
        inv_val<-solve(data, ...)
        x$setinverse(inv_val)
        inv_val
}
